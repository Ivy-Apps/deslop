// Violates service-may-only-open-its-own-internal. The folder is spelled
// 'DBConnection' but the canonical PascalCase spelling of 'db-connection' is
// 'DbConnection', and an allows clause accepts only the canonical spelling -
// exempting more than was written would silence real violations.
import { secret } from '@/internal/DBConnection/secret';

export const dbConnection = secret;
