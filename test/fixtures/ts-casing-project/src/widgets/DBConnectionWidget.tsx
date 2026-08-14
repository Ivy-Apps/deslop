// The name is captured only in PascalCase, so kebab-case is derived. A run of
// capitals reads as one word, giving 'db-connection' - the module that exists.
import { config } from '@/config/db-connection';

export function DBConnectionWidget() {
  return config.name;
}
