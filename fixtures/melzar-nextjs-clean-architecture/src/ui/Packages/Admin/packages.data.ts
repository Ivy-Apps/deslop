import { fetchPackages } from 'ui/shared/Packages/Admin/packages.actions';
import { Package } from 'presentation/shared/Packages/models/Package';

type UsePackagesData = {
  getPackages: () => Promise<Package[]>;
};

export const usePackagesData = (): UsePackagesData => {
  const getPackages = async () => {
    return fetchPackages();
  };

  return {
    getPackages,
  };
};
