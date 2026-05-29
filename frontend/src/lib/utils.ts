import { clsx, type ClassValue } from "clsx";
import { twMerge } from "tailwind-merge";

/**
 * `cn(...)` — shadcn/ui's standard class-name helper.
 *
 * Merges Tailwind utility classes intelligently (later classes win,
 * conflicting utilities are deduped). Use everywhere you'd otherwise
 * concatenate `className` strings.
 */
export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs));
}
