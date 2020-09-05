#pragma once

#include "Modules/ModuleManager.h"
#include "Engine/Texture2D.h"

class FUserFlagDirectory;
struct FFlagType {
	FString Name;
	FString HangableName;
	uint32_t HeightMultiplier;
};

class FUserFlag;

class FFLAGGONCustomModule : public FDefaultGameModuleImpl {
public:
	virtual void StartupModule() override;
	virtual bool IsGameModule() const override { return true; }

private:
	void AddTexturesToManager(UObject * ManagerObject, const FFlagType& FlagType);
	void RemoveTexturesFromManager(UObject * ManagerObject, const FFlagType& FlagType);
	void HookInteractable(UWorld* World, const FFlagType& FlagType);
	void Initialize(UWorld *World);
	UTexture2D* FindTexture(const FString& Name);
	void UpdateFolders();
	bool bInitialized = false;
	bool bInterceptReferences = true;

	TArray<FUserFlagDirectory> Folders;
	TMap<FString, FFlagType> FlagTypes;
	TMap<FString, FUserFlag*> FlagsByHash;
	
};