import React from 'react';

import { PageHeading } from 'ui/common/components/layout/Page/PageHeading/PageHeading';
import { useClientPageHeading } from 'ui/Clients/Admin/Client/components/ClientPageHeading/clientPageHeading.hooks';

type Props = {
  clientId: string;
};
/** @scope src/ui/Clients/Admin/Client */
export const ClientPageHeading = async ({ clientId }: Props) => {
  const { client } = await useClientPageHeading({ clientId });

  return (
    <PageHeading>
      {client?.firstName} {client?.lastName}
    </PageHeading>
  );
};
