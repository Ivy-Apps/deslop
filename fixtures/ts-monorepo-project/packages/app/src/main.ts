import { Button } from '@ui/Button';
import { formatMoney } from './helpers/format';

export function main(): string {
  return Button(formatMoney(42));
}
