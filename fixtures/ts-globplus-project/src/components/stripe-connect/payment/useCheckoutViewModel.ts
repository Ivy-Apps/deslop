// Conforms: calls its own provider's service module.
import { capture } from '@/services/stripe-connect/payment-checkout';

export function useCheckoutViewModel() {
  return { state: { amount: capture() } };
}
