import { interfaces } from 'inversify';

import { ApplicationContainer } from 'ioc/application.container';

/** @scope src/ioc */
export const applyDependencies = (
  func: Function,
  dependencies: interfaces.ServiceIdentifier<unknown>[] = []
) => {
  const injections = dependencies.map((dependency) =>
    ApplicationContainer.get(dependency)
  );
  return func.apply(func, injections);
};
