import { useDashboardData } from 'ui/Dashboard/Admin/dashboard.data';
import { Package } from 'presentation/shared/Packages/models/Package';

type UseDashboard = {
  packagesRequiresAttention: number;
  deliveredPackages: Package[];
  recentPackages: Package[];
  packagesInTransit: Package[];
};
export const useDashboard = async (): Promise<UseDashboard> => {
  const { getRecentPackages } = useDashboardData();

  const packages = await getRecentPackages();

  const packagesRequiresAttention = packages.filter(
    (pack: Package) => pack.status === 'attention_needed'
  ).length;

  const recentPackages = packages.filter(
    (pack: Package) => pack.status === 'processing'
  );
  const packagesInTransit = packages.filter(
    (pack: Package) => pack.status === 'in_transit'
  );
  const deliveredPackages = packages.filter(
    (pack: Package) => pack.status === 'delivered'
  );

  return {
    packagesRequiresAttention,
    recentPackages,
    deliveredPackages,
    packagesInTransit,
  };
};
