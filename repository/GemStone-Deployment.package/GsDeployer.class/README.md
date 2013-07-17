For now only persistent instances are migrated.

During the deploy operation:

  1. autoCommit and autoMigrate suspended
  2. commitOnAlmostOutOfMemory enabled
  3. Warnings caught and resumed
  4. bulk migrate of classes with classHistory performed after deployBlock finished