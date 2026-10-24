import { Client } from 'presentation/shared/Clients/models/Client';
import { useClientData } from 'ui/Clients/Admin/Client/client.data';

type UseClientPageHeading = {
  client: Client;
};

type UseClientPageHeadingInput = {
  clientId: string;
};

/** @scope src/ui/Clients/Admin/Client/components/ClientPageHeading */
export const useClientPageHeading = async ({
  clientId,
}: UseClientPageHeadingInput): Promise<UseClientPageHeading> => {
  const { getClient } = useClientData();

  const client = await getClient(clientId);

  return { client };
};
