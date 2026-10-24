// The negative control for the acronym class. 'db-pool' and 'DBPool' are one
// name, so the rule DOES apply here - and this View imports the wrong config,
// so it is reported. Without that report, a silently skipped acronym folder
// would look exactly like a satisfied one.
import { config } from '@/config/http-cache';

export function DBPoolView() {
  return config.name;
}
