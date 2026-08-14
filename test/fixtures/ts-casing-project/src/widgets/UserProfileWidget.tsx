// Control: no acronym, so deriving kebab-case from PascalCase is exact.
import { config } from '@/config/user-profile';

export function UserProfileWidget() {
  return config.name;
}
