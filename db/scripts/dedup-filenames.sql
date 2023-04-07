/*
 If a filename with the same name already exists but a different hash, the moved file can either:
 * Create a new version (newer created)
 * Create another version (don't change created of either, one will be newer than the other)
 * Not be moved
 * Get a new name (eg _1)

 If a filename exists with the same name AND same hash, creating a new version that has the same content is a waste of space.
 Instead, set the created to min(created) and the accessed to max(accessed), or maybe now().
 */