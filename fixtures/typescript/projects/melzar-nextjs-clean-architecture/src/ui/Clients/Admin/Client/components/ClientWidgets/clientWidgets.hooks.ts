import { Package } from 'presentation/shared/Packages/models/Package';
import { useClientData } from 'ui/Clients/Admin/Client/client.data';

type UseClientWidgets = {
  packagesRequiresAttention: number;
  recentPackages: number;
  packagesInTransit: number;
  deliveredPackages: number;
};

type UseClientWidgetsInput = {
  clientId: string;
};
/** @scope src/ui/Clients/Admin/Client/components/ClientWidgets */
export const useClientWidgets = async ({
  clientId,
}: UseClientWidgetsInput): Promise<UseClientWidgets> => {
  const { getClientPackages } = useClientData();

  const packages = await getClientPackages(clientId);

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
    recentPackages: recentPackages.length,
    packagesInTransit: packagesInTransit.length,
    deliveredPackages: deliveredPackages.length,
  };
};
