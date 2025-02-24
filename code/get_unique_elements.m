function unique_elements = get_unique_elements (elements)

% Find the unique elements while preserving the original order
[~, uniqueIdx] = unique(elements, 'stable');
unique_elements = elements(sort(uniqueIdx));

end