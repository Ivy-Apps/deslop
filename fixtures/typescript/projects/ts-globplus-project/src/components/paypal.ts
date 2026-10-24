// The provider's own module, sitting beside its folder rather than inside it.
//
// Violates providers-are-isolated: reaches into the stripe-connect provider.
// The rule targets "@/components/{{provider-name}}/**", and this module is
// what the ** stands for when it stands for nothing at all - so the rule
// applies here exactly as it does one level down.
import { CheckoutView } from '@/components/stripe-connect/payment/CheckoutView';

export function paypal() {
  return CheckoutView();
}
