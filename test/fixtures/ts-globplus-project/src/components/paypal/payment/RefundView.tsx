// Violates view-wires-own-view-model: never imports useRefundViewModel.
// Violates providers-are-isolated: reaches into the stripe-connect provider.
// Violates view-has-a-storybook: there is no RefundView.stories file.
import { CheckoutView } from '@/components/stripe-connect/payment/CheckoutView';

export function RefundView() {
  return CheckoutView();
}
