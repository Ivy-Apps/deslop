// Two acronym words running together; the kebab folder pins the split.
import { config } from '@/config/aws-s3';

export function AWSS3View() {
  return config.name;
}
