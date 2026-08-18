// VIOLATION: a hook reaching the UI. This single import pulls in five
// forbidden component modules, which must be reported as one problem.
import { ItemListViewProps } from '@/components/list/ItemListView';

export function useItemList(): ItemListViewProps {
  return { items: [] };
}
