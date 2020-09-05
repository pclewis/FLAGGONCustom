#include "FLAGGONCustomModule.h"
#include "mod/blueprint_hooking.h"
#include "util/Logging.h"
#include "FGGameInstance.h"
#include "mod/hooking.h"
#include "mod/blueprint_hooking.h"
#include "Misc/DateTime.h"


#include "IImageWrapperModule.h"
#include "IImageWrapper.h"

#include "FGUseableInterface.h"
#include "FGObjectReference.h"

//#include "DirectoryWatcherModule.h"
//#include "IDirectoryWatcher.h"

constexpr const TCHAR* trim_dir(const TCHAR* path) {
	const TCHAR* result = path;
	for (; *path; ++path)
	{
		if (*path == '/' || *path == '\\')
		{
			result = path+1;
		}
	}
	return result;
}

#undef ERROR
#define LOG(level,msg,...) SML::Logging::level(*FString::Printf(TEXT("[FLAGGONCustom] (%s:%d): %s: " msg), trim_dir(TEXT(__FILE__)), __LINE__, TEXT(__FUNCTION__), __VA_ARGS__))
#define DEBUG(msg,...)   LOG(debug,   msg, __VA_ARGS__)
#define INFO(msg,...)    LOG(info,    msg, __VA_ARGS__)
#define WARNING(msg,...) LOG(warning, msg, __VA_ARGS__)
#define ERROR(msg,...)   LOG(error,   msg, __VA_ARGS__)


class FStructArray {
public:
	FStructArray(UArrayProperty* ArrayProperty, void* Container) :
		ArrayHelper(FScriptArrayHelper(ArrayProperty, ArrayProperty->ContainerPtrToValuePtr<void>(Container)))
	{
		UScriptStruct* Struct = Cast<UStructProperty>(ArrayProperty->Inner)->Struct;

		for (UProperty* Property = Struct->PropertyLink; Property != nullptr; Property = Property->PropertyLinkNext)
		{
			FString PropertyName = Property->GetName();
			int32 UnderscoreIndex = -1;
			if (PropertyName.FindChar('_', UnderscoreIndex)) {
				PropertyName = PropertyName.Left(UnderscoreIndex);
			}
			Properties.Add(PropertyName, Property);
		}
	}

	template <typename T>
	typename T::TCppType* Get(uint32 Index, const FString& PropertyName)
	{
		T* Property = Cast<T>(*Properties.Find(PropertyName));
		check(Property);
		return Property->GetPropertyValuePtr_InContainer(ArrayHelper.GetRawPtr(Index));
	}

	template <typename T>
	void Set(uint32 Index, const FString& PropertyName, const typename T::TCppType& Value)
	{
		if (UProperty** Property = Properties.Find(PropertyName)) {
			if (T* TypedProperty = Cast<T>(*Property)) {
				TypedProperty->SetPropertyValue_InContainer(ArrayHelper.GetRawPtr(Index), Value);
			}
		}
	}


	FScriptArrayHelper ArrayHelper;
	TMap<FString, UProperty*> Properties;
};


class FAddonManager {
private:

public:
	FAddonManager(UObject* AddonManagerActor, UArrayProperty* TextureArrayProperty, UArrayProperty* CatsArrayProperty) :
		Textures(FStructArray(TextureArrayProperty, AddonManagerActor)),
		Categories(FStructArray(CatsArrayProperty, AddonManagerActor))
	{

	}

	FAddonManager(UObject* AddonManagerActor, const FString& TypeName) :
		FAddonManager(
			AddonManagerActor,
			Cast<UArrayProperty>(AddonManagerActor->GetClass()->FindPropertyByName(*(TypeName + TEXT(" Texture Array")))),
			Cast<UArrayProperty>(AddonManagerActor->GetClass()->FindPropertyByName(*(TypeName + TEXT(" Texture Cats")))))
	{
	}

	FStructArray Textures;
	FStructArray Categories;
};

class FUserFlag
{
public:
	FString Path;
	FDateTime LastModified = FDateTime::MinValue();
	TArray<uint8> FileData;
	TMap<FString, TSoftObjectPtr<UTexture2D>> Textures;
	EImageFormat ImageFormat;
	uint32 Width = 0;
	uint32 Height = 0;
	FString Hash;

	FUserFlag(const FString& Path) : Path(Path)	{}

	bool NeedsUpdate() const {
		return IFileManager::Get().GetTimeStamp(*Path) > LastModified;
	}

	void Update() {
		if (!NeedsUpdate())
		{
			return;
		}

		LastModified = IFileManager::Get().GetTimeStamp(*Path);

		if (!FFileHelper::LoadFileToArray(FileData, *Path)) {
			ERROR("%s: Could not read", *Path);
			return;
		}

		IImageWrapperModule& ImageWrapperModule = FModuleManager::LoadModuleChecked<IImageWrapperModule>(FName("ImageWrapper"));
		ImageFormat = ImageWrapperModule.DetectImageFormat(FileData.GetData(), FileData.Num());

		if (ImageFormat == EImageFormat::Invalid) {
			WARNING("%s: Unknown image format", *Path);
			return;
		}

		uint64_t ContentHash = CityHash64((const char*)FileData.GetData(), FileData.Num());
		Hash = FString::Printf(TEXT("%016llx"), ContentHash);
	}

	bool IsValid() const {
		return ImageFormat != EImageFormat::Invalid;
	}

	UTexture2D* CreateTexture(const FFlagType& FlagType)
	{
		if (!IsValid())
		{
			return nullptr;
		}

		FString TextureName = FString::Printf(TEXT("/Game/FLAGGONCustom/%s/%s"), *FlagType.HangableName, *Hash);

		TSoftObjectPtr<UTexture2D>* ExistingPtr = Textures.Find(FlagType.Name);
		if (ExistingPtr != nullptr && ExistingPtr->IsValid())
		{
			INFO("Found existing valid softptr to %s, returning it", *TextureName);
			return ExistingPtr->Get();
		}

		IImageWrapperModule& ImageWrapperModule = FModuleManager::LoadModuleChecked<IImageWrapperModule>(FName("ImageWrapper"));
		TSharedPtr<IImageWrapper> ImageWrapper = ImageWrapperModule.CreateImageWrapper(ImageFormat);
		check(ImageWrapper.IsValid());

		verify(ImageWrapper->SetCompressed(FileData.GetData(), FileData.Num()));

		Width = ImageWrapper->GetWidth();
		Height = ImageWrapper->GetHeight();

		// HACK: doing this after loading Width and Height so we don't divide by zero later
		UTexture2D* Existing = FindObject<UTexture2D>(GetTransientPackage(), *TextureName);
		if (Existing) {
			INFO("Found existing asset named %s, returning", *TextureName);
			Textures.Add(FlagType.Name, Existing);
			return Existing;
		}

		INFO("%s: No existing texture found, making a new one", *TextureName);

		const TArray<uint8>* UncompressedBGRA = NULL;
		verify(ImageWrapper->GetRaw(ERGBFormat::BGRA, 8, UncompressedBGRA));

		int HeightMultiplier = FlagType.HeightMultiplier;
		if (HeightMultiplier <= 0) { // calculate dynamically for 2x1 stacked
			// cart ad: 1000x250 (4:1), expanded to 1000x1000, 4 repeats, Scale=2
			// nyan:    4000x500 (8:1), expanded to 4000x4000, 2 repeats, Scale=4

			// scale=1 --- 8   repeats --- 2:1    MIN
			// scale=2 --- 4   repeats --- 4:1
			// scale=3 --- 2.6 repeats --- 6:1    INVALID (leftmost 1.3 on each panel)
			// scale=4 --- 2   repeats --- 8:1    MAX (screen is 16:1 in two panels)

			// We must add blank rows to make the image squareish, because reasons.
				
			float Ratio = FMath::Clamp<float>( (float)Width / (float)Height, 2, 8 );
			HeightMultiplier = pow(2, log2(Ratio));
		}

		UTexture2D* NewTexture = UTexture2D::CreateTransient(Width, Height * HeightMultiplier, PF_B8G8R8A8);

		uint8* TextureData = (uint8*)NewTexture->PlatformData->Mips[0].BulkData.Lock(LOCK_READ_WRITE);
		FMemory::Memcpy(TextureData, UncompressedBGRA->GetData(), UncompressedBGRA->Num());
		NewTexture->PlatformData->Mips[0].BulkData.Unlock();

		NewTexture->UpdateResource();

		NewTexture->Rename(*TextureName);

		Textures.Add( FlagType.Name, NewTexture );

		INFO("%s loaded: %d x %d", *Path, NewTexture->GetSizeX(), NewTexture->GetSizeY());

		return NewTexture;
	}
};

class FUserFlagDirectory
{
public:
	FString Name;
	FString Path;
	TMap<FString, FUserFlag> Flags;

	FUserFlagDirectory(const FString& Name, const FString& Path) : Name(Name), Path(Path)
	{
	}

	void Update()
	{
		TArray<FString> FileNames;
		IFileManager::Get().FindFilesRecursive(
			FileNames,
			*Path,
			TEXT("*"),   // Match all files
			true,        // Include files
			false);      // Don't include directories

		for (const FString& FileName : FileNames) {
			FUserFlag* Existing = Flags.Find(FileName);
			if (Existing)
			{
				DEBUG("%s: Updating existing flag", *FileName);
				Existing->Update();
			}
			else
			{
				DEBUG("%s: Adding new flag", *FileName);
				FUserFlag& Flag = Flags.Add(FileName, FUserFlag(FileName));
				Flag.Update();
			}
		}
	}
};


void FFLAGGONCustomModule::AddTexturesToManager(UObject* ManagerObject, const FFlagType& FlagType)
{
	DEBUG("Populating menu in manager %s", *ManagerObject->GetPathName());
	FAddonManager Manager(ManagerObject, FlagType.Name);

	for (FUserFlagDirectory& Folder : Folders) {
		DEBUG("Adding textures from %s", *Folder.Name);

		Folder.Update();

		int32 CategoryIndex = Manager.Categories.ArrayHelper.AddValue();
		int32 StartPos = Manager.Textures.ArrayHelper.Num();

		Manager.Categories.Set<UTextProperty>(CategoryIndex, TEXT("CatName"), FText::AsCultureInvariant(Folder.Name));
		Manager.Categories.Set<UTextProperty>(CategoryIndex, TEXT("CatAuthor"), FText::AsCultureInvariant(TEXT("you did this")));
		Manager.Categories.Set<UIntProperty>(CategoryIndex, TEXT("StartPos"), StartPos);

		for (TPair<FString, FUserFlag>& FlagEntry : Folder.Flags)
		{
			FUserFlag& Flag = FlagEntry.Value;
			if (Flag.IsValid()) {
				UTexture2D* Texture = FlagEntry.Value.CreateTexture(FlagType);
				if (Texture == nullptr) 
				{
					WARNING("Flag IsValid but CreateTexture was null?? %s", *Flag.Path);
				}
				else
				{
					int32 TextureIndex = Manager.Textures.ArrayHelper.AddValue();
					Manager.Textures.Set<UTextProperty>(TextureIndex, TEXT("Name"), FText::AsCultureInvariant(FlagEntry.Key));
					Manager.Textures.Set<UTextProperty>(TextureIndex, TEXT("Description"), FText::AsCultureInvariant(FlagEntry.Key));
					Manager.Textures.Set<UObjectProperty>(TextureIndex, TEXT("Texture"), Texture);

					int Scale = Texture->GetSizeY() / FlagEntry.Value.Height / 2;
					Manager.Textures.Set<UIntProperty>(TextureIndex, TEXT("Scale"), Scale);
				}
			}
		}
	}
}

// HACK: just delete our stuff instead of trying to manage it properly.
void FFLAGGONCustomModule::RemoveTexturesFromManager(UObject* ManagerObject, const FFlagType& FlagType)
{
	DEBUG("Populating menu in manager %s", *ManagerObject->GetPathName());
	FAddonManager Manager(ManagerObject, FlagType.Name);

	int32 NumCats = Manager.Categories.ArrayHelper.Num();
	int32 NumTextures = Manager.Textures.ArrayHelper.Num();

	for (FUserFlagDirectory& Folder : Folders) {
		NumCats -= 1;
		for (const TPair<FString, FUserFlag>& FlagEntry : Folder.Flags)
		{
			if (FlagEntry.Value.IsValid()) {
				NumTextures -= 1;
			}
		}
	}

	Manager.Categories.ArrayHelper.Resize(NumCats);
	Manager.Textures.ArrayHelper.Resize(NumTextures);
}


void FFLAGGONCustomModule::HookInteractable(UWorld* World, const FFlagType& FlagType)
{
	FString ClassName = FString::Printf(TEXT("/Game/FLAGSMod/Blueprints/Interact_Hangable_%s.Interact_Hangable_%s_C"), *FlagType.HangableName, *FlagType.HangableName);
	UClass* Interactable = LoadObject<UClass>(nullptr, *ClassName);
	if (Interactable)
	{
		UFunction* Function = Interactable->FindFunctionByName(TEXT("Populate Menu"));
		if (Function) {
			INFO("%s: Found Populate Menu function, hooking", *ClassName);

			// Note: must hook return address first, or it will crash
			HookBlueprintFunction(Function,
				[this, ClassName, FlagType, World](FBlueprintHookHelper& helper) {
					DEBUG("%s::'Populate Menu': exit", *ClassName);
					UObject* ManagerObject = *helper.GetLocalVarPtr<UObjectProperty>(TEXT("Addon Manager"));
					RemoveTexturesFromManager(ManagerObject, FlagType);
				},
				EPredefinedHookOffset::Return);

			HookBlueprintFunction(Function,
				[this, ClassName, FlagType, World](FBlueprintHookHelper& helper) {
					DEBUG("%s::'Populate Menu': enter", *ClassName);
					UObject* ManagerObject = *helper.GetLocalVarPtr<UObjectProperty>(TEXT("Addon Manager"));
					bInterceptReferences = false;
					AddTexturesToManager(ManagerObject, FlagType);
					bInterceptReferences = true;
				},
				EPredefinedHookOffset::Start);
		}
		else
		{
			ERROR("%s: Couldn't find Populate Menu function", *ClassName);
		}
	}
	else
	{
		ERROR("Couldn't find interactable for type: %s (%s)", *FlagType.Name, *FlagType.HangableName);
	}
}


UObject* FindAddonManager(UWorld *World) {
	for (TActorIterator<AActor> ActorIterator(World); ActorIterator; ++ActorIterator)
	{
		AActor *Actor = *ActorIterator;

		if (Actor->GetClass()->GetPathName().StartsWith(TEXT("/Game/FLAGSMod/Blueprints/FLAGS_ADDON_MANAGER"))) {
			INFO("Found addon manager: %s", *Actor->GetClass()->GetPathName());
			return Actor;
		}
	}

	ERROR("/Game/FLAGSMod/Blueprints/FLAGS_ADDON_MANAGER not found, is FLAGSMod installed?");

	return nullptr;
}


void FFLAGGONCustomModule::Initialize(UWorld *World)
{
	DEBUG("Initializing");
	for (TPair<FString, FFlagType> Pair : FlagTypes) {
		FFlagType& FlagType = Pair.Value;
		HookInteractable(World, FlagType);
	}

	bInitialized = true;
}

void FFLAGGONCustomModule::UpdateFolders()
{
	for (FUserFlagDirectory& Folder : Folders)
	{
		Folder.Update();
		for (TPair<FString, FUserFlag>& Pair : Folder.Flags)
		{
			FUserFlag& Flag = Pair.Value;
			if (Flag.IsValid()) {
				FlagsByHash.Add(Flag.Hash, &Flag);
			}
		}
	}
}

UTexture2D* FFLAGGONCustomModule::FindTexture(const FString& Name) 
{
	TArray<FString> Elements;
	Name.ParseIntoArray(Elements, TEXT("/"));
	if (Elements.Num() != 6) { // Engine, Transient., Game, FLAGGONCustom, Type, Hash
		WARNING("Invalid path: %s", *Name);
		return nullptr;
	}
	FString& FlagTypeName = Elements[4];
	FString& FlagHash = Elements[5];
	FFlagType* FlagType = FlagTypes.Find(FlagTypeName);
	FUserFlag** Flag = FlagsByHash.Find(FlagHash);

	if (FlagType != nullptr && Flag != nullptr) {
		return (*Flag)->CreateTexture(*FlagType);
	}

	return nullptr;
}


#define JSON(x) TEXT(#x)

TSharedPtr<FJsonObject> CreateConfigDefaults() {
	FString defaultConfig = JSON({"folders": ["%USERDIR%/My Games/FactoryGame/flags"]});
	return SML::ParseJsonLenient(defaultConfig);
}

void FFLAGGONCustomModule::StartupModule()
{
	DEBUG("Starting up");
	TSharedRef<FJsonObject> Config = SML::ReadModConfig("FLAGGONCustom", CreateConfigDefaults().ToSharedRef());

	TArray<TSharedPtr<FJsonValue>> ConfigFolders = Config->GetArrayField("folders");

	FPaths::ProjectUserDir();

	if (ConfigFolders.Num() == 1 && ConfigFolders[0]->AsString().Equals( TEXT("%USERDIR%/../Pictures") ) )
	{
		WARNING("Replacing 0.0.1 default config with new default config.");
		Config = CreateConfigDefaults().ToSharedRef();
		SML::WriteModConfig("FLAGGONCustom", Config);
		ConfigFolders = Config->GetArrayField("folders");
	}

	FlagTypes.Add(TEXT("1x1"),          FFlagType{ TEXT("1x1"),         TEXT("1x1"),          1 });
	FlagTypes.Add(TEXT("2x1"),          FFlagType{ TEXT("2x1"),         TEXT("2x1"),          2 });
	FlagTypes.Add(TEXT("Stacked_2x1a"), FFlagType{ TEXT("Stacked 2x1"), TEXT("Stacked_2x1a"), 0 });

	for (TSharedPtr<FJsonValue> Folder : ConfigFolders)
	{
		FString Path = Folder->AsString().Replace(TEXT("%USERDIR%"), FPlatformProcess::UserDir());
		FPaths::CollapseRelativeDirectories(Path);
		FPaths::RemoveDuplicateSlashes(Path);
		INFO("Adding folder: %s", *Path);
		Folders.Add(FUserFlagDirectory(*Path, *Path));
	}

	SUBSCRIBE_METHOD(UWorld::BeginPlay, [this](auto& _scope, UWorld* World) {
		DEBUG("UWorld::BeginPlay");
		if (!bInitialized) Initialize(World);
		UpdateFolders();
		bInterceptReferences = true;
	});

	/*
	SUBSCRIBE_METHOD(StaticLoadObject, [](auto& scope, UClass* ObjectClass, UObject* InOuter, const TCHAR* InName, const TCHAR* Filename, uint32 LoadFlags, UPackageMap* Sandbox, bool bAllowObjectReconciliation, FUObjectSerializeContext* InSerializeContext) {
		INFO("StaticLoadObject: %s", InName);
	});
	*/

	SUBSCRIBE_METHOD(StaticFindObject, [this](auto& Scope, UClass* ObjectClass, UObject* InObjectPackage, const TCHAR* OrigInName, bool ExactClass) {
		//DEBUG("StaticFindObject: %s", OrigInName);
		if (bInterceptReferences && wcsncmp(OrigInName, TEXT("/Engine/Transient./Game/FLAGGONCustom/"), 38 ) == 0) {
			INFO("StaticFindObject: Intercepting request for %s", OrigInName);
			bInterceptReferences = false;
			UTexture2D *Result = FindTexture(OrigInName);
			if (Result) {
				Scope.Override(Result);
			}
			bInterceptReferences = true;
		}
	});


	/*
	SUBSCRIBE_METHOD(FObjectReferenceDisc::ResolveWithRedirect, [](auto& scope, FObjectReferenceDisc* self, UWorld* world, const FString* outerName, UObject** out_object, UObject** out_outer)
	{	
		DEBUG("FGObjectReferenceDisc::ResolveWithRedirect");
	});
	*/

	//HookInvoker<decltype(&MethodReference), &MethodReference, None>::addHandlerBefore(Handler);



	/*
	SUBSCRIBE_METHOD(UFGGameInstance::LoadComplete, [this](auto& _scope, UFGGameInstance* GameInstance, float _loadTime, const FString& _mapName) {
		DEBUG("UFGGameInstance::LoadComplete");
		bInterceptReferences = false;
	});
	*/
}

IMPLEMENT_GAME_MODULE(FFLAGGONCustomModule, FLAGGONCustom);
