// Not a target of any rule; it exists to put a hop between a gateway and the
// @/internal folder it must not reach.
import { deep } from '@/internal/HTTPClient/deep';

export const relay = deep;
