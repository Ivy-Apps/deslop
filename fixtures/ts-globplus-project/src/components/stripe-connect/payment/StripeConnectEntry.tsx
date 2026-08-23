// The entry point is named after its provider folder, so the repeated
// {{provider-name}} / {{ProviderName}} variable binds across the **.
import { STRIPE_CONNECT } from '@/config/stripe-connect';

export function StripeConnectEntry() {
  return STRIPE_CONNECT.apiVersion;
}
