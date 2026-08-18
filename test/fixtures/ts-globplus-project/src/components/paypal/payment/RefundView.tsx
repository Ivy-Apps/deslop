// Violates view-wires-own-view-model: never imports useRefundViewModel.
// Violates providers-are-isolated: reaches into the stripe-connect provider.
// Violates view-has-a-storybook: there is no RefundView.stories file.
// The shared formatter it imports is its OWN provider's, one directory back
// from this file - {{TARGET_DIR}}/../shared/ is per-provider, not global.
import { CheckoutView } from '@/components/stripe-connect/payment/CheckoutView';
import { formatMoney } from '@/components/paypal/shared/format-money';
import { STRIPE_CONNECT } from '@/config/stripe-connect';

export function RefundView() {
  return CheckoutView() + formatMoney(1);
}
