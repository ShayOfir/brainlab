function new_elements = remove_item (elements, elementsToRemove)

    new_elements = elements(~ismember(elements, elementsToRemove));