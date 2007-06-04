using System;
using System.Collections;
using NUnit.Framework;

namespace kana
{
	[TestFixture]
	public class CharacterModelTest
	{
	    [Test]
		public void TestScriptTypes()
		{
		    CharacterModel cm = new CharacterModel();
		    Assert.AreEqual(cm.scriptType('あ'), ScriptType.Hiragana);
		    Assert.AreEqual(cm.scriptType('ア'), ScriptType.Katakana);
		    Assert.AreEqual(cm.scriptType('阿'), ScriptType.Han);
		    Assert.AreEqual(cm.scriptType('a'), ScriptType.Ascii);
		    return;
		}
		
		[Test]
		public void TestScriptBoundaries()
		{
		    CharacterModel cm = new CharacterModel();
		    ArrayList expected = new ArrayList();
		    expected.Add(new TypedSegment("寿司", ScriptType.Han));
		    expected.Add(new TypedSegment("を", ScriptType.Hiragana));
		    expected.Add(new TypedSegment("食", ScriptType.Han));
		    expected.Add(new TypedSegment("べたい", ScriptType.Hiragana));
		    
		    ArrayList result = cm.scriptBoundaries("寿司を食べたい");
		    Assert.AreEqual(expected.Count, result.Count);
		    for (int i = 0; i < expected.Count; i++)
		    {
		        Assert.AreEqual(expected[i], result[i]);
		    }
		}
	}
}
