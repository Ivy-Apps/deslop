// Conforms: wires its own ViewModel and has a Storybook file.
import { useCheckoutViewModel } from '@/components/stripe-connect/payment/useCheckoutViewModel';
import { STRIPE_CONNECT } from '@/config/stripe-connect';

export function CheckoutView() {
  const { state } = useCheckoutViewModel();
  return state.amount;
}
