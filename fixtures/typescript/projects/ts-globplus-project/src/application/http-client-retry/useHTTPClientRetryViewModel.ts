// Conforms, and the three-word name carries an acronym: 'http-client-retry'
// and 'HTTPClientRetry' are one name, so the rule applies here too.
import { HTTPClientRetryUseCase } from '@/application/http-client-retry/HTTPClientRetryUseCase';

export function useHTTPClientRetryViewModel() {
  return HTTPClientRetryUseCase();
}
