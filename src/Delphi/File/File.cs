using System;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override File VisitFile(DelphiParser.FileContext cx)
    {
        return (cx.unit(), cx.program(), cx.library(), cx.packageE()) switch
        {
            ({} unit, _, _, _) => VisitUnit(unit),
            (_, {} program, _, _) => VisitProgram(program),
            // (_, _, {} library, _) => VisitLibrary(library),
            // (_, _, _, {} package) => VisitPackageE(package),
            _ => throw new ArgumentOutOfRangeException(nameof(cx))
        };
    }
}

public abstract class File() : DelphiNode(DelphiNodeKind.File)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitFile(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitFile(this);
} 


