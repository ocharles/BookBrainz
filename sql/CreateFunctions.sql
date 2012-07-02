BEGIN;

SET LOCAL SESSION AUTHORIZATION bookbrainz;
SET LOCAL search_path = 'bookbrainz', 'public';

CREATE FUNCTION text_is_normalized(inp text)
RETURNS boolean AS $$
  BEGIN
    RETURN inp ~ E'^[[:print:]]+$'
      AND inp !~ E'\\s{2,}'
      AND inp = btrim(inp);
  END;
$$ LANGUAGE 'plpgsql';
COMMENT ON FUNCTION text_is_normalized(text) IS $$
  Ensures that text is non-empty, contains only printable characters, does not
  begin or end with spaces, and that there are not runs of more than 1 whitespace
  character.
$$;

COMMIT;
