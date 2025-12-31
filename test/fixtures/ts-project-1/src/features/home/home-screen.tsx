import React from 'react';
// Testing relative import resolution:
import { getHomeData } from "./home-component"; 
// Testing cross-directory relative import resolution:
import { Button } from "../../components/ds/Button";

/**
 * AI-generated: Home Screen layout.
 * This component fetches home data and renders the DS button.
 */
export const HomeScreen: React.FC = () => {
  const homeData = getHomeData();

  const handleAction = () => {
    // AI: Placeholder for analytics tracking
    console.log("Button clicked on Home Screen");
    alert(`Action for: ${homeData.title}`);
  };

  return (
    <div className="home-screen-container">
      <header>
        <h1>{homeData.title}</h1>
      </header>
      
      <section>
        <p>Welcome back! Your last update was: {homeData.lastUpdated}</p>
        
        {/* AI: Example of using the DS Button with props */}
        <Button 
          label="Refresh Dashboard" 
          variant="primary" 
          onClick={handleAction} 
        />
      </section>
    </div>
  );
};