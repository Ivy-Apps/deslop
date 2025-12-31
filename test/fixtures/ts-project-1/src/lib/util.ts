/**
 * AI-generated utility functions for string manipulation.
 * This comment block should be removed by the tool.
 */

export const formatDate = (date: Date): string => {
  // 1. Logic to format date to ISO string
  return date.toISOString().split('T')[0];
};

export const capitalize = (str: string): string => {
  return str.charAt(0).toUpperCase() + str.slice(1);
};