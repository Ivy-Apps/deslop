// VIOLATION: inherits the whole forbidden subgraph one hop further out.
import { useItemList } from '@/hooks/list/useItemList';

export function testUseItemList(): void {
  useItemList();
}
