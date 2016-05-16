I unconditionally bypass instance migration. 
Install me if you want to do your own class migration using tools like tODE that make calls to GsDeployer:

  GsNonmigratingDeployer select;
  System commit.

To temporarily suspend migrations, used GsDeployer class>>noMigrationDuring:.