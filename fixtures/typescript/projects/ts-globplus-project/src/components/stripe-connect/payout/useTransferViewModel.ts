// Violates view-model-calls-its-own-provider-service: this is the payment
// service, but the ViewModel lives under the payout service type.
import { capture } from '@/services/stripe-connect/payment-checkout';

export function useTransferViewModel() {
  return { state: { amount: capture() } };
}
