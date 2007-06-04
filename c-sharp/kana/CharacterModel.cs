using System;
using System.Collections;

namespace kana
{
    public enum ScriptType {
    	Hiragana,
    	Katakana,
    	Han,
    	Ascii,
    	FullAscii,
    	Unknown,
    }

    public class TypedSegment
    {
        public TypedSegment(string segValue, ScriptType segScript)
        {
            value = segValue;
            script = segScript;
        }
        
        public override bool Equals(object o)
        {
            if (o == null)
            {
                return false;
            }
            
            TypedSegment rhs = o as TypedSegment;
            if (rhs == null)
            {
                return false;
            }
            
            return value == rhs.value && script == rhs.script;
        }
        
        public bool Equals(TypedSegment rhs)
        {
            return value == rhs.value && script == rhs.script;
        }
        
        public override int GetHashCode()
        {
            return value.GetHashCode() ^ (int) script;
        }
        
        // The actual string segment.
        public string value;
        
        // The script of this segment.
    	public ScriptType script;
    }

    public class CharacterModel
    {
    	private const int _interKanaDistance = 96;      // ord(ア) - ord(あ)
    	private const char _hiraganaStarts = '\u3041'; // ぁ character
    	private const char _hiraganaEnds = '\u3096';   // hiragana ヶ character
    	private const char _katakanaStarts = '\u30a1'; // ァ character
    	private const char _katakanaEnds = '\u30f6';   // ヶ character
    	private const char _fullAsciiStarts = '\uff01'; // ！ character
    	private const char _fullAsciiEnds = '\uff5f'; // ｟ character
    	private const int _asciiEnds = 128; 
    	
    	public CharacterModel()
    	{
    	}
    	
    	// Returns the script type of a given character.
    	public ScriptType scriptType(char c)
    	{
    		if (c <= (char) _asciiEnds)
    		{
    			return ScriptType.Ascii;
    		}
    		else if (_hiraganaStarts <= c && c <= _hiraganaEnds)
    		{
    			return ScriptType.Hiragana;
    		}
    		else if (_katakanaStarts <= c && c <= _katakanaEnds)
    		{
    			return ScriptType.Katakana;
    		}
    		else if (
                (c >= '\u4e00' && c <= '\u9fff') || // CJK unified ideographs
                (c >= '\u3400' && c <= '\u4dbf') || // CJK unified ideographs ext A
                (c >= '\u3200' && c <= '\u32ff') || // Enclosed CJK letters
                (c >= '\u3300' && c <= '\u33ff') || // CJK compatibility
                (c >= '\uf900' && c <= '\ufaff') || // CJK compatibility ideographs
                false
            )
            {
            	return ScriptType.Han;
            }
            else if (_fullAsciiStarts <= c && c <= _fullAsciiEnds)
            {
                return ScriptType.FullAscii;
            }
            else
    		{
    			return ScriptType.Unknown;
    		}
    	}
    	
    	// Returns a list of segments and their boundaries.
    	public ArrayList scriptBoundaries(string input)
    	{
    		ArrayList result = new ArrayList();
    		if (input.Length == 0)
    		{
    			return result;
    		}
    		string thisSegment = input[0].ToString();
    		ScriptType thisScript = scriptType(thisSegment[0]);
    		ScriptType script;
    		
    		for (int i = 1; i < input.Length; i++)
    		{
    			script = scriptType(input[i]);
    			if (script == thisScript)
    			{
    			    // Continue the old segment.
    				thisSegment += input[i];
    			}
    			else
    			{
    			    // Finish the old segment.
    			    result.Add(new TypedSegment(thisSegment, thisScript));
    			    
    			    // Start a new one.
    			    thisSegment = input[i].ToString();
    			    thisScript = script;
    			}
    		}
    		
    		// Add the last segment found.
		    result.Add(new TypedSegment(thisSegment, thisScript));
    	    
    	    return result;
    	}
    }
}