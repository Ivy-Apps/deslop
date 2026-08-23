import React from 'react';

import { ClientView } from 'ui/Clients/Admin/Client/client.view';

/** @scope src/app */
export default function Client({ params }: { params: { id: string } }) {
  return <ClientView clientId={params.id} />;
}
