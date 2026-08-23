import React from 'react';
// Testing deep relative imports
import { getHomeData } from '../../features/home/home-component';

/**
 * AI-generated: Main Home Page Component
 * This needs to be cleaned up.
 */
export default function HomePage() {
  const data = getHomeData();

  return (
    <main>
      <h1>{data.title}</h1>
      <p>System Date: {data.lastUpdated}</p>
      {/* AI: Add more widgets here later */}
    </main>
  );
}