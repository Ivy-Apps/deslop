import { describe, expect, it } from 'vitest';
import { redirects } from './next.config';

describe('redirects', () => {
  it('should be sorted alphabetically by source', () => {
    const sources = redirects.map((r) => r.source);
    const sortedSources = [...sources].sort();
    expect(sources).toEqual(sortedSources);
  });
});
