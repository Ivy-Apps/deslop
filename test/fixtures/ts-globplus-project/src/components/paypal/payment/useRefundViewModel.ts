// Conforms: calls its own provider's service module.
import { refund } from '@/services/paypal/payment-refund';

export function useRefundViewModel() {
  return { state: { amount: refund() } };
}
