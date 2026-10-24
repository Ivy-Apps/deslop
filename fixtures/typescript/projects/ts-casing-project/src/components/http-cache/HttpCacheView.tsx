// Control: no acronym, so this always matched.
import { config } from '@/config/http-cache';

export function HttpCacheView() {
  return config.name;
}
