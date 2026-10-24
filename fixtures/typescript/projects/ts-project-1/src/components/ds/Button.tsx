import React from 'react';

// AI: Interface for Button props
interface ButtonProps extends React.ButtonHTMLAttributes<HTMLButtonElement> {
  variant?: 'primary' | 'secondary';
  label: string;
}

/**
 * AI-generated Button component for the Design System.
 * Should be used across all features.
 */
export const Button: React.FC<ButtonProps> = ({ 
  variant = 'primary', 
  label, 
  ...props 
}) => {
  const style = {
    padding: '10px 20px',
    backgroundColor: variant === 'primary' ? 'blue' : 'gray',
    color: 'white',
    borderRadius: '4px',
    border: 'none',
    cursor: 'pointer',
  };

  /* AI: Logic for rendering the button element */
  return (
    <button style={style} {...props}>
      {label}
    </button>
  );
};