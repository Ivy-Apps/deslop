import { fetchHomeData } from "@/features/home/data/home-repository";

// VIOLATION of features-cant-import-other-features:
// {{TARGET_DIR}} = @/features/auth, so @/features/auth/** does NOT match @/features/home/data/home-repository.
// The allows clause does not cover cross-feature imports.
export const getBridgedData = () => fetchHomeData();
