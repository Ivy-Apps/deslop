// Conforms: wires its own ViewModel, has a Storybook file, and reaches only
// its own service folder and the provider's shared folder one directory back.
import { useCheckoutViewModel } from '@/components/stripe-connect/payment/useCheckoutViewModel';
import { Money } from '@/components/stripe-connect/shared/Money';
import { formatMoney } from '@/components/stripe-connect/shared/format-money';
import { STRIPE_CONNECT } from '@/config/stripe-connect';

export function CheckoutView() {
  const { state } = useCheckoutViewModel();
  return Money({ amount: formatMoney(state.amount) });
}
