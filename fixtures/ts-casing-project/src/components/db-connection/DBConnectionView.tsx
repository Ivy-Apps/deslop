// Acronym at the front.
import { config } from '@/config/db-connection';

export function DBConnectionView() {
  return config.name;
}
