// Violates gateway-keeps-out-of-internal-transitively. It does not import
// @/internal/HTTPClient itself - lib/relay does, one hop further - and the
// folder is spelled the acronym way, so only a forbidding clause that widens
// over every spelling of 'http-client' catches it.
import { relay } from '@/lib/relay';

export const httpClient = relay;
