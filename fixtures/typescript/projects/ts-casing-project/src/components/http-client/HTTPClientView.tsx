// Acronym glued to a word: HTTPClient and http-client are one name.
import { config } from '@/config/http-client';

export function HTTPClientView() {
  return config.name;
}
