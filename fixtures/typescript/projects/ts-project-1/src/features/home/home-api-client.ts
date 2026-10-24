import { fetchHomeData } from "@/features/home/data/home-repository";

// ALLOWED by features-cant-import-other-features:
// {{TARGET_DIR}} = @/features/home, so @/features/home/** matches @/features/home/data/home-repository.
export const getApiData = () => fetchHomeData();
