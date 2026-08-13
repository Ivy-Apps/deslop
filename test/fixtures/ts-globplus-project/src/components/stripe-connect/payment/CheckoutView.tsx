// Conforms: wires its own ViewModel and has a Storybook file.
import { useCheckoutViewModel } from '@/components/stripe-connect/payment/useCheckoutViewModel';

export function CheckoutView() {
  const { state } = useCheckoutViewModel();
  return state.amount;
}
