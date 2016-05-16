private
mcPlatformSupportDo: aBlock
  "Set autoCommit to true, autoMigrate to false (if doing a bulk migrate .. see 
   shouldAutoMigrateDuringDeploy), then invoke block using commitOnAlmostOutOfMemoryDuring:.
   Restore autoCommit and autoMigrate to original values."

  | oldAutoCommit oldAutoMigrate |
  oldAutoCommit := MCPlatformSupport autoCommit.
  oldAutoMigrate := MCPlatformSupport autoMigrate.
  MCPlatformSupport autoCommit: true.
  MCPlatformSupport autoMigrate: self shouldAutoMigrateDuringDeploy.
  [ MCPlatformSupport commitOnAlmostOutOfMemoryDuring: aBlock ]
    ensure: [ 
      MCPlatformSupport autoCommit: oldAutoCommit.
      MCPlatformSupport autoMigrate: oldAutoMigrate ]