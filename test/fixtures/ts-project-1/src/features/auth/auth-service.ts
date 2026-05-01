import { capitalize } from "../../lib/util";
import { mockUser } from '../../tests/fixtures';

// VIOLATION of no-tests-import-in-features: imports test fixtures in production code.
export const getAuthUser = () => {
  const name = capitalize(mockUser.name);
  return { name };
};
