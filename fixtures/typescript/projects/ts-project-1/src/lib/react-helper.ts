import React from 'react';

// VIOLATION of no-react-in-lib: directly imports React in the lib layer.
// Library modules must be framework-agnostic; React code belongs in components.
export const createEmptyElement = (): React.ReactElement => {
  return React.createElement('span', null);
};
