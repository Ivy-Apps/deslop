import { ItemCard } from '@/components/list/ItemCard';
import { Banner } from '@/components/ds/Banner';
import { typography } from '@/components/ds/typography';
import { classNames } from '@/components/ds/class-names';

export interface ItemListViewProps {
  items: string[];
}

export function ItemListView({ items }: ItemListViewProps): string {
  return classNames(typography.body, Banner(items.length), ItemCard());
}
