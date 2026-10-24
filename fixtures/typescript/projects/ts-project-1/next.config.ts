import type { NextConfig } from 'next';
import createNextIntlPlugin from 'next-intl/plugin';
import { buildCsp } from '@/lib/security';

export const redirects = [
  {
    source: '/legacy-page',
    destination: '/',
    permanent: true,
  },
]
