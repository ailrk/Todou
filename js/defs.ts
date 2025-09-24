export type EntryId = number;


export type Visibility = "All" | "Completed" | "Active";


export interface Entry {
  id: EntryId;
  description: string;
  completed: boolean;
  editing: boolean;
}


export interface Model {
  entries: Entry[];
  visibility: Visibility;
  field: string;
  nextId: EntryId;
}
