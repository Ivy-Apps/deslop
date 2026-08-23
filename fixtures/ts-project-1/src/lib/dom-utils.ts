import React from 'react';

// VIOLATION of no-react-in-lib: directly imports React in the lib layer.
export const renderToString = (element: React.ReactElement): string => {
  return String(element.type);
};
