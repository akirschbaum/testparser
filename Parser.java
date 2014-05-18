import java.io.IOException;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import org.parboiled.BaseParser;
import org.parboiled.Parboiled;
import org.parboiled.Rule;
import org.parboiled.annotations.Cached;
import org.parboiled.buffers.DefaultInputBuffer;
import org.parboiled.buffers.InputBuffer;
import org.parboiled.errors.ParseError;
import org.parboiled.parserunners.ReportingParseRunner;
import org.parboiled.support.ParsingResult;
import org.parboiled.support.Position;
import org.parboiled.support.Var;

public class Parser {

    public static void main(final String[] args) throws IOException {
        if (args.length != 1) {
            System.err.println("usage: C-file");
            System.exit(1);
            throw new AssertionError();
        }
        final SourceLocations sourceLocations = new SourceLocations();
        final CParser parser = Parboiled.createParser(CParser.class, sourceLocations);
        final Var<SourceNode> translationUnit = new Var<>();
        final Rule rule = parser.TranslationUnit(translationUnit);
        final InputBuffer buffer = new DefaultInputBuffer(new String(Files.readAllBytes(Paths.get(args[0])), "UTF-8").toCharArray());
        final ParsingResult<?> result = new ReportingParseRunner<>(rule).run(buffer);
        if (result.hasErrors()) {
            for (final ParseError parseError : result.parseErrors) {
                final String errorMessage = parseError.getErrorMessage();
                final int startIndex = parseError.getStartIndex();
                System.err.println(startIndex + ": " + (errorMessage == null ? "parse error" : errorMessage));
            }
        }
        if (!result.matched) {
            throw new IOException("parsing failed");
        }
        final SourceNode sourceNode = translationUnit.get();
    }

    public static class CParser extends BaseParser<SourceNode> {

        public static final Collection<String> KEYWORDS;

        static {
            final Collection<String> keywords = new HashSet<>();
            keywords.add("asm");
            keywords.add("auto");
            keywords.add("break");
            keywords.add("case");
            keywords.add("char");
            keywords.add("const");
            keywords.add("continue");
            keywords.add("default");
            keywords.add("do");
            keywords.add("double");
            keywords.add("else");
            keywords.add("enum");
            keywords.add("extern");
            keywords.add("float");
            keywords.add("for");
            keywords.add("goto");
            keywords.add("if");
            keywords.add("inline");
            keywords.add("int");
            keywords.add("long");
            keywords.add("register");
            keywords.add("restrict");
            keywords.add("return");
            keywords.add("short");
            keywords.add("signed");
            keywords.add("sizeof");
            keywords.add("static");
            keywords.add("struct");
            keywords.add("switch");
            keywords.add("typedef");
            keywords.add("union");
            keywords.add("unsigned");
            keywords.add("void");
            keywords.add("volatile");
            keywords.add("while");
            keywords.add("_Bool");
            keywords.add("_Complex");
            keywords.add("_Imaginary");
            KEYWORDS = Collections.unmodifiableCollection(keywords);
        }

        private final SourceLocations sourceLocations;

        public CParser(final SourceLocations sourceLocations) {
            this.sourceLocations = sourceLocations;
        }

        public Rule Identifier() {
            return Sequence(
                Sequence(
                    IdentifierNondigit(),
                    ZeroOrMore(
                        IdentifierCharacter()
                    )
                ),
                push(new Identifier(getSourceLocation(), getContext().getMatch())),
                TestNot(
                    IdentifierCharacter()
                ),
                WhiteSpace()
            );
        }

        public Rule IdentifierNondigit() {
            return FirstOf(
                AnyOf("abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ$"),
                UniversalCharacterName()
            );
        }

        public Rule IdentifierCharacter() {
            return FirstOf(
                AnyOf("abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$"),
                UniversalCharacterName()
            );
        }

        public Rule Digit() {
            return CharRange('0', '9');
        }

        public Rule UniversalCharacterName() {
            return FirstOf(
                Sequence("\\u", HexQuad()),
                Sequence("\\U", HexQuad(), HexQuad())
            );
        }

        public Rule HexQuad() {
            return Sequence(
                HexadecimalDigit(),
                HexadecimalDigit(),
                HexadecimalDigit(),
                HexadecimalDigit()
            );
        }

        public Rule Constant() {
            return FirstOf(
                FloatingConstant(),
                IntegerConstant(),
                CharacterConstant()
            );
        }

        public Rule IntegerConstant() {
            return Sequence(
                FirstOf(
                    HexadecimalConstant(),
                    OctalConstant(),
                    DecimalConstant()
                ),
                WhiteSpace()
            );
        }

        public Rule DecimalConstant() {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> integerSuffix = new Var<>();
            final Var<BigInteger> value = new Var<>();
            return Sequence(
                Sequence(
                    CharRange('1', '9'),
                    sourceLocation.set(getSourceLocation()),
                    ZeroOrMore(
                        CharRange('0', '9')
                    )
                ),
                parseIntegerConstant(value, getContext().getMatch(), 10),
                TestNot(
                    CharRange('0', '9')
                ),
                Optional(
                    IntegerSuffix(),
                    integerSuffix.set((SourceNode) pop())
                ),
                push(new SourceNode(sourceLocation.get(), value.get(), integerSuffix.isSet() ? integerSuffix.get() : BooleanMarker.FALSE))
            );
        }

        public Rule OctalConstant() {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> integerSuffix = new Var<>();
            final Var<BigInteger> value = new Var<>();
            return Sequence(
                Sequence(
                    '0',
                    sourceLocation.set(getSourceLocation()),
                    ZeroOrMore(
                        OctalDigit()
                    )
                ),
                parseIntegerConstant(value, getContext().getMatch(), 8),
                TestNot(
                    OctalDigit()
                ),
                Optional(
                    IntegerSuffix(),
                    integerSuffix.set((SourceNode) pop())
                ),
                push(new SourceNode(sourceLocation.get(), value.get(), integerSuffix.isSet() ? integerSuffix.get() : new SourceNode(getSourceLocation())))
            );
        }

        public Rule OctalDigit() {
            return CharRange('0', '7');
        }

        public Rule HexadecimalConstant() {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> integerSuffix = new Var<>();
            final Var<BigInteger> value = new Var<>();
            return Sequence(
                HexadecimalPrefix(),
                sourceLocation.set(getSourceLocation()),
                OneOrMore(
                    HexadecimalDigit()
                ),
                parseIntegerConstant(value, getContext().getMatch(), 16),
                TestNot(
                    HexadecimalDigit()
                ),
                Optional(
                    IntegerSuffix(),
                    integerSuffix.set((SourceNode) pop())
                ),
                push(new SourceNode(sourceLocation.get(), value.get(), integerSuffix.isSet() ? integerSuffix.get() : new SourceNode(getSourceLocation())))
            );
        }

        public Rule HexadecimalPrefix() {
            return FirstOf("0x", "0X");
        }

        public Rule HexadecimalDigit() {
            return FirstOf(
                CharRange('0', '9'),
                CharRange('a', 'f'),
                CharRange('A', 'F')
            );
        }

        public Rule IntegerSuffix() {
            return FirstOf(
                Sequence("llu", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("llU", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("ll", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("lu", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("lU", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("l", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("LLu", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("LLU", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("LL", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("Lu", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("LU", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("L", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("ull", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("uLL", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("ul", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("uL", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("u", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("Ull", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("ULL", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("Ul", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("UL", push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence("U", push(new SourceNode(getSourceLocation(), getContext().getMatch())))
            );
        }

        public Rule FloatingConstant() {
            return Sequence(
                FirstOf(
                    DecimalFloatingConstant(),
                    HexadecimalFloatingConstant()
                ),
                WhiteSpace()
            );
        }

        public Rule DecimalFloatingConstant() {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> floatingSuffix = new Var<>();
            return FirstOf(
                Sequence(
                    FractionalConstant(),
                    sourceLocation.set(getSourceLocation()),
                    Optional(
                        ExponentPart()
                    ),
                    Optional(
                        FloatingSuffix(),
                        floatingSuffix.set((SourceNode) pop())
                    ),
                    push(new SourceNode(sourceLocation.get(), floatingSuffix.isSet() ? floatingSuffix.get() : new SourceNode(getSourceLocation())))
                ),
                Sequence(
                    DigitSequence(),
                    sourceLocation.set(getSourceLocation()),
                    ExponentPart(),
                    Optional(
                        FloatingSuffix(),
                        floatingSuffix.set((SourceNode) pop())
                    ),
                    push(new SourceNode(sourceLocation.get(), floatingSuffix.isSet() ? floatingSuffix.get() : new SourceNode(getSourceLocation())))
                )
            );
        }

        public Rule HexadecimalFloatingConstant() {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> floatingSuffix = new Var<>();
            return Sequence(
                HexadecimalPrefix(),
                sourceLocation.set(getSourceLocation()),
                FirstOf(
                    Sequence(
                        HexadecimalDigitSequence(),
                        Optional(
                            '.',
                            Optional(
                                HexadecimalDigitSequence()
                            )
                        )
                    ),
                    Sequence(
                        '.',
                        HexadecimalDigitSequence()
                    )
                ),
                BinaryExponentPart(),
                Optional(
                    FloatingSuffix(),
                    floatingSuffix.set((SourceNode) pop())
                ),
                push(new SourceNode(sourceLocation.get(), floatingSuffix.isSet() ? floatingSuffix.get() : new SourceNode(getSourceLocation())))
            );
        }

        public Rule FractionalConstant() {
            return FirstOf(
                Sequence(
                    Optional(
                        DigitSequence()
                    ),
                    '.',
                    DigitSequence()
                ),
                Sequence(
                    DigitSequence(),
                    '.'
                )
            );
        }

        public Rule ExponentPart() {
            return Sequence(
                FirstOf('e', 'E'),
                Optional(
                    Sign()
                ),
                DigitSequence()
            );
        }

        public Rule Sign() {
            return FirstOf('+', '-');
        }

        public Rule DigitSequence() {
            return Sequence(
                OneOrMore(
                    Digit()
                ),
                TestNot(
                    Digit()
                )
            );
        }

        public Rule BinaryExponentPart() {
            return Sequence(
                FirstOf('p', 'P'),
                Optional(
                    Sign()
                ),
                DigitSequence()
            );
        }

        public Rule HexadecimalDigitSequence() {
            return Sequence(
                OneOrMore(
                    HexadecimalDigit()
                ),
                TestNot(
                    HexadecimalDigit()
                )
            );
        }

        public Rule FloatingSuffix() {
            return FirstOf(
                Sequence('f', push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence('l', push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence('F', push(new SourceNode(getSourceLocation(), getContext().getMatch()))),
                Sequence('L', push(new SourceNode(getSourceLocation(), getContext().getMatch())))
            );
        }

        public Rule EnumerationConstant() {
            final Var<SourceLocation> sourceLocation = new Var<>();
            return Sequence(
                Identifier(),
                sourceLocation.set(getSourceLocation()),
                isNonKeyword((Identifier) peek()),
                push(new SourceNode(sourceLocation.get(), (Identifier) pop()))
            );
        }

        public Rule CharacterConstant() {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<Boolean> longCharacterConstant = new Var<>();
            return Sequence(
                FirstOf(
                    Sequence("L'", longCharacterConstant.set(true)),
                    Sequence("'", longCharacterConstant.set(false))
                ),
                sourceLocation.set(getSourceLocation()),
                CCharSequence(),
                "' ",
                push(new SourceNode(sourceLocation.get(), longCharacterConstant.get()))
            );
        }

        public Rule CCharSequence() {
            return OneOrMore(
                CChar()
            );
        }

        public Rule CChar() {
            return FirstOf(
                EscapeSequence(),
                NoneOf("'\\\n")
            );
        }

        public Rule EscapeSequence() {
            return FirstOf(
                SimpleEscapeSequence(),
                OctalEscapeSequence(),
                HexadecimalEscapeSequence(),
                UniversalCharacterName()
            );
        }

        public Rule SimpleEscapeSequence() {
            return FirstOf("\\'", "\\\"", "\\?", "\\\\", "\\a", "\\b", "\\f", "\\n", "\\r", "\\t", "\\v");
        }

        public Rule OctalEscapeSequence() {
            return FirstOf(
                Sequence('\\', OctalDigit(), OctalDigit(), OctalDigit()),
                Sequence('\\', OctalDigit(), OctalDigit()),
                Sequence('\\', OctalDigit())
            );
        }

        public Rule HexadecimalEscapeSequence() {
            return Sequence(
                "\\x",
                OneOrMore(
                    HexadecimalDigit()
                ),
                WhiteSpace()
            );
        }

        public Rule StringLiteral() {
            return Sequence(
                StringLiteralSingle(),
                ZeroOrMore(
                    StringLiteralSingle(),
                    push(new SourceNode((SourceNode) pop(1), (SourceNode) pop()))
                )
            );
        }

        public Rule StringLiteralSingle() {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<Boolean> wide = new Var<>();
            return Sequence(
                FirstOf(
                    Sequence("\"", wide.set(false)),
                    Sequence("L\"", wide.set(true))
                ),
                sourceLocation.set(getSourceLocation()),
                Optional(
                    SCharSequence()
                ),
                "\" ",
                push(new SourceNode(sourceLocation.get(), wide.get()))
            );
        }

        public Rule SCharSequence() {
            return OneOrMore(
                SChar()
            );
        }

        public Rule SChar() {
            return FirstOf(
                EscapeSequence(),
                NoneOf("\"\\\n")
            );
        }

        @Cached
        public Rule PrimaryExpression(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<Identifier> identifier = new Var<>();
            return FirstOf(
                Sequence(
                    Constant(),
                    push(new SourceNode(getSourceLocation(), (SourceNode) pop()))
                ),
                Sequence(
                    StringLiteral(),
                    push(new SourceNode(getSourceLocation(), (SourceNode) pop()))
                ),
                Sequence(
                    Identifier(),
                    identifier.set((Identifier) pop()),
                    sourceLocation.set(getSourceLocation()),
                    isNonKeyword(identifier.get()),
                    push(new SourceNode(sourceLocation.get(), identifier.get()))
                ),
                Sequence(
                    "( ",
                    Expression(typedefNames),
                    ") "
                ),
                Sequence(
                    "( ",
                    sourceLocation.set(getSourceLocation()),
                    CompoundStatement(typedefNames),
                    ") ",
                    push(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                )
            );
        }

        @Cached
        public Rule PostfixExpression(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<Identifier> identifier = new Var<>();
            final Var<SourceNode> expr = new Var<>();
            final Var<SourceNode> typeName = new Var<>();
            final Var<SourceNode> argumentExpressionList = new Var<>();
            return Sequence(
                FirstOf(
                    Sequence(
                        PrimaryExpression(typedefNames),
                        expr.set((SourceNode) pop())
                    ),
                    Sequence(
                        "( ",
                        sourceLocation.set(getSourceLocation()),
                        TypeName(typedefNames),
                        typeName.set((SourceNode) pop()),
                        ") ",
                        "{ ",
                        InitializerList(typedefNames),
                        Optional(
                            ", "
                        ),
                        "} ",
                        expr.set(new SourceNode(sourceLocation.get(), typeName.get(), new SourceNode(sourceLocation.get(), new SourceNode(sourceLocation.get(), BigInteger.ZERO, new SourceNode(sourceLocation.get())))))
                    )
                ),
                sourceLocation.set(getSourceLocation()),
                ZeroOrMore(
                    FirstOf(
                        Sequence(
                            "[ ",
                            Expression(typedefNames),
                            "] ",
                            expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop()))
                        ),
                        Sequence(
                            "( ",
                            Optional(
                                ArgumentExpressionList(typedefNames),
                                argumentExpressionList.set((SourceNode) pop())
                            ),
                            argumentExpressionList.isSet() || argumentExpressionList.set(new SourceNode(getSourceLocation())),
                            ") ",
                            expr.set(new SourceNode(sourceLocation.get(), expr.get(), argumentExpressionList.get()))
                        ),
                        Sequence(
                            ". ",
                            Identifier(), identifier.set((Identifier) pop()), isNonKeyword(identifier.get()),
                            expr.set(new SourceNode(sourceLocation.get(), expr.get(), identifier.get()))
                        ),
                        Sequence(
                            "-> ",
                            Identifier(), identifier.set((Identifier) pop()), isNonKeyword(identifier.get()),
                            expr.set(new SourceNode(sourceLocation.get(), expr.get(), identifier.get()))
                        ),
                        Sequence(
                            "++ ",
                            expr.set(new SourceNode(sourceLocation.get(), expr.get()))
                        ),
                        Sequence(
                            "-- ",
                            expr.set(new SourceNode(sourceLocation.get(), expr.get()))
                        )
                    )
                ),
                push(expr.get())
            );
        }

        public Rule ArgumentExpressionList(final Var<TypedefNames> typedefNames) {
            final Var<SourceNode> argumentExpressionList = new Var<>();
            return Sequence(
                AssignmentExpression(typedefNames),
                argumentExpressionList.set(new SourceNode(getSourceLocation(), (SourceNode) pop())),
                ZeroOrMore(
                    ", ",
                    AssignmentExpression(typedefNames),
                    argumentExpressionList.set(new SourceNode(argumentExpressionList.get(), (SourceNode) pop()))
                ),
                push(argumentExpressionList.get())
            );
        }

        @Cached
        public Rule UnaryExpression(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            return FirstOf(
                PostfixExpression(typedefNames),
                Sequence(
                    "++ ",
                    sourceLocation.set(getSourceLocation()),
                    UnaryExpression(typedefNames),
                    push(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                ),
                Sequence(
                    "-- ",
                    sourceLocation.set(getSourceLocation()),
                    UnaryExpression(typedefNames),
                    push(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                ),
                Sequence(
                    "&",
                    TestNot("&"),
                    WhiteSpace(),
                    sourceLocation.set(getSourceLocation()),
                    CastExpression(typedefNames),
                    drop(),
                    push(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                ),
                Sequence(
                    "* ",
                    sourceLocation.set(getSourceLocation()),
                    CastExpression(typedefNames),
                    drop(),
                    push(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                ),
                Sequence(
                    "+",
                    TestNot("+"),
                    WhiteSpace(),
                    sourceLocation.set(getSourceLocation()),
                    CastExpression(typedefNames),
                    drop(),
                    push(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                ),
                Sequence(
                    "-",
                    TestNot("-"),
                    WhiteSpace(),
                    sourceLocation.set(getSourceLocation()),
                    CastExpression(typedefNames),
                    drop(),
                    push(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                ),
                Sequence(
                    "~ ",
                    sourceLocation.set(getSourceLocation()),
                    CastExpression(typedefNames),
                    drop(),
                    push(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                ),
                Sequence(
                    "! ",
                    sourceLocation.set(getSourceLocation()),
                    CastExpression(typedefNames),
                    drop(),
                    push(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                ),
                Sequence(
                    Keyword("sizeof"),
                    sourceLocation.set(getSourceLocation()),
                    UnaryExpression(typedefNames),
                    push(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                ),
                Sequence(
                    Keyword("sizeof"),
                    sourceLocation.set(getSourceLocation()),
                    "( ",
                    TypeName(typedefNames),
                    ") ",
                    push(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                )
            );
        }

        @Cached
        public Rule UnaryExpression(final Var<SourceNode> expr, final Var<TypedefNames> typedefNames) {
            return Sequence(
                UnaryExpression(typedefNames),
                expr.set((SourceNode) pop())
            );
        }

        @Cached
        public Rule CastExpression(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> typeName = new Var<>();
            return FirstOf(
                Sequence(
                    "( ",
                    sourceLocation.set(getSourceLocation()),
                    TypeName(typedefNames),
                    typeName.set((SourceNode) pop()),
                    ") ",
                    CastExpression(typedefNames),
                    drop(),
                    push(new SourceNode(sourceLocation.get(), typeName.get(), (SourceNode) pop())),
                    push(new SourceNode(getSourceLocation()))
                ),
                Sequence(
                    UnaryExpression(typedefNames),
                    push(new SourceNode(getSourceLocation()))
                )
            );
        }

        @Cached
        public Rule MultiplicativeExpression(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> expr = new Var<>();
            final Var<BooleanMarker> booleanMarker = new Var<>();
            return Sequence(
                CastExpression(typedefNames),
                booleanMarker.set((BooleanMarker) pop()),
                sourceLocation.set(getSourceLocation()),
                expr.set((SourceNode) pop()),
                ZeroOrMore(
                    FirstOf(
                        Sequence(
                            "* ",
                            CastExpression(typedefNames),
                            drop(),
                            expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop())),
                            booleanMarker.set(BooleanMarker.FALSE)
                        ),
                        Sequence(
                            "/ ",
                            CastExpression(typedefNames),
                            drop(),
                            expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop())),
                            booleanMarker.set(BooleanMarker.FALSE)
                        ),
                        Sequence(
                            "% ",
                            CastExpression(typedefNames),
                            drop(),
                            expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop())),
                            booleanMarker.set(BooleanMarker.FALSE)
                        )
                    )
                ),
                push(expr.get()),
                push(booleanMarker.get())
            );
        }

        @Cached
        public Rule AdditiveExpression(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> expr = new Var<>();
            final Var<BooleanMarker> booleanMarker = new Var<>();
            return Sequence(
                MultiplicativeExpression(typedefNames),
                booleanMarker.set((BooleanMarker) pop()),
                sourceLocation.set(getSourceLocation()),
                expr.set((SourceNode) pop()),
                ZeroOrMore(
                    FirstOf(
                        Sequence(
                            "+",
                            TestNot("+"),
                            WhiteSpace(),
                            MultiplicativeExpression(typedefNames),
                            drop(),
                            expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop())),
                            booleanMarker.set(BooleanMarker.FALSE)
                        ),
                        Sequence(
                            "-",
                            TestNot("-"),
                            WhiteSpace(),
                            MultiplicativeExpression(typedefNames),
                            drop(),
                            expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop())),
                            booleanMarker.set(BooleanMarker.FALSE)
                        )
                    )
                ),
                push(expr.get()),
                push(booleanMarker.get())
            );
        }

        @Cached
        public Rule ShiftExpression(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> expr = new Var<>();
            final Var<BooleanMarker> booleanMarker = new Var<>();
            return Sequence(
                AdditiveExpression(typedefNames),
                booleanMarker.set((BooleanMarker) pop()),
                sourceLocation.set(getSourceLocation()),
                expr.set((SourceNode) pop()),
                ZeroOrMore(
                    FirstOf(
                        Sequence(
                            "<< ",
                            AdditiveExpression(typedefNames),
                            drop(),
                            expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop())),
                            booleanMarker.set(BooleanMarker.FALSE)
                        ),
                        Sequence(
                            ">> ",
                            AdditiveExpression(typedefNames),
                            drop(),
                            expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop())),
                            booleanMarker.set(BooleanMarker.FALSE)
                        )
                    )
                ),
                push(expr.get()),
                push(booleanMarker.get())
            );
        }

        @Cached
        public Rule RelationalExpression(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> expr = new Var<>();
            final Var<BooleanMarker> booleanMarker = new Var<>();
            return Sequence(
                ShiftExpression(typedefNames),
                booleanMarker.set((BooleanMarker) pop()),
                sourceLocation.set(getSourceLocation()),
                expr.set((SourceNode) pop()),
                ZeroOrMore(
                    FirstOf(
                        Sequence(
                            "< ",
                            ShiftExpression(typedefNames),
                            drop(),
                            expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop())),
                            booleanMarker.set(BooleanMarker.FALSE)
                        ),
                        Sequence(
                            "> ",
                            ShiftExpression(typedefNames),
                            drop(),
                            expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop())),
                            booleanMarker.set(BooleanMarker.FALSE)
                        ),
                        Sequence(
                            "<= ",
                            ShiftExpression(typedefNames),
                            drop(),
                            expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop())),
                            booleanMarker.set(BooleanMarker.FALSE)
                        ),
                        Sequence(
                            ">= ",
                            ShiftExpression(typedefNames),
                            drop(),
                            expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop())),
                            booleanMarker.set(BooleanMarker.FALSE)
                        )
                    )
                ),
                push(expr.get()),
                push(booleanMarker.get())
            );
        }

        @Cached
        public Rule EqualityExpression(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> expr = new Var<>();
            final Var<BooleanMarker> booleanMarker = new Var<>();
            return Sequence(
                RelationalExpression(typedefNames),
                booleanMarker.set((BooleanMarker) pop()),
                sourceLocation.set(getSourceLocation()),
                expr.set((SourceNode) pop()),
                ZeroOrMore(
                    FirstOf(
                        Sequence(
                            "== ",
                            RelationalExpression(typedefNames),
                            drop(),
                            expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop())),
                            booleanMarker.set(BooleanMarker.FALSE)
                        ),
                        Sequence(
                            "!= ",
                            RelationalExpression(typedefNames),
                            drop(),
                            expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop())),
                            booleanMarker.set(BooleanMarker.FALSE)
                        )
                    )
                ),
                push(expr.get()),
                push(booleanMarker.get())
            );
        }

        @Cached
        public Rule AndExpression(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> expr = new Var<>();
            final Var<BooleanMarker> booleanMarker = new Var<>();
            return Sequence(
                EqualityExpression(typedefNames),
                booleanMarker.set((BooleanMarker) pop()),
                sourceLocation.set(getSourceLocation()),
                expr.set((SourceNode) pop()),
                ZeroOrMore(
                    "&",
                    TestNot("&"),
                    WhiteSpace(),
                    EqualityExpression(typedefNames),
                    drop(),
                    expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop())),
                    booleanMarker.set(BooleanMarker.FALSE)
                ),
                push(expr.get()),
                push(booleanMarker.get())
            );
        }

        @Cached
        public Rule ExclusiveOrExpression(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> expr = new Var<>();
            final Var<BooleanMarker> booleanMarker = new Var<>();
            return Sequence(
                AndExpression(typedefNames),
                booleanMarker.set((BooleanMarker) pop()),
                sourceLocation.set(getSourceLocation()),
                expr.set((SourceNode) pop()),
                ZeroOrMore(
                    "^ ",
                    AndExpression(typedefNames),
                    drop(),
                    expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop())),
                    booleanMarker.set(BooleanMarker.TRUE)
                ),
                push(expr.get()),
                push(booleanMarker.get())
            );
        }

        @Cached
        public Rule InclusiveOrExpression(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> expr = new Var<>();
            final Var<BooleanMarker> booleanMarker = new Var<>();
            return Sequence(
                ExclusiveOrExpression(typedefNames),
                booleanMarker.set((BooleanMarker) pop()),
                sourceLocation.set(getSourceLocation()),
                expr.set((SourceNode) pop()),
                ZeroOrMore(
                    "| ",
                    ExclusiveOrExpression(typedefNames),
                    drop(),
                    expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop())),
                    booleanMarker.set(BooleanMarker.FALSE)
                ),
                push(expr.get()),
                push(booleanMarker.get())
            );
        }

        @Cached
        public Rule LogicalAndExpression(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> expr = new Var<>();
            final Var<BooleanMarker> booleanMarker = new Var<>();
            return Sequence(
                InclusiveOrExpression(typedefNames),
                booleanMarker.set((BooleanMarker) pop()),
                sourceLocation.set(getSourceLocation()),
                expr.set((SourceNode) pop()),
                ZeroOrMore(
                    "&& ",
                    InclusiveOrExpression(typedefNames),
                    drop(),
                    expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop())),
                    booleanMarker.set(BooleanMarker.FALSE)
                ),
                push(expr.get()),
                push(booleanMarker.get())
            );
        }

        @Cached
        public Rule LogicalOrExpression(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> expr = new Var<>();
            final Var<BooleanMarker> booleanMarker = new Var<>();
            return Sequence(
                LogicalAndExpression(typedefNames),
                booleanMarker.set((BooleanMarker) pop()),
                sourceLocation.set(getSourceLocation()),
                expr.set((SourceNode) pop()),
                ZeroOrMore(
                    "|| ",
                    LogicalAndExpression(typedefNames),
                    drop(),
                    expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop())),
                    booleanMarker.set(BooleanMarker.FALSE)
                ),
                push(expr.get()),
                push(booleanMarker.get())
            );
        }

        @Cached
        public Rule ConditionalExpression(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> conditionExpr = new Var<>();
            final Var<SourceNode> trueExpr = new Var<>();
            return Sequence(
                LogicalOrExpression(typedefNames),
                Optional(
                    sourceLocation.set(getSourceLocation()),
                    drop(),
                    conditionExpr.set((SourceNode) pop()),
                    "? ",
                    Expression(typedefNames),
                    trueExpr.set((SourceNode) pop()),
                    ": ",
                    ConditionalExpression(typedefNames),
                    drop(),
                    push(new SourceNode(sourceLocation.get(), conditionExpr.get(), trueExpr.get(), (SourceNode) pop())),
                    push(BooleanMarker.FALSE)
                )
            );
        }

        @Cached
        public Rule AssignmentExpression(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> expr = new Var<>();
            final Var<BooleanMarker> booleanMarker = new Var<>();
            return Sequence(
                ConditionalExpression(typedefNames),
                sourceLocation.set(getSourceLocation()),
                booleanMarker.set((BooleanMarker) pop()),
                Optional(
                    booleanMarker.get().getValue(),
                    expr.set((SourceNode) pop()),
                    FirstOf(
                        Sequence(
                            "= ",
                            AssignmentExpression(typedefNames),
                            push(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop()))
                        ),
                        Sequence(
                            "*= ",
                            AssignmentExpression(typedefNames),
                            push(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop()))
                        ),
                        Sequence(
                            "/= ",
                            AssignmentExpression(typedefNames),
                            push(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop()))
                        ),
                        Sequence(
                            "%= ",
                            AssignmentExpression(typedefNames),
                            push(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop()))
                        ),
                        Sequence(
                            "+= ",
                            AssignmentExpression(typedefNames),
                            push(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop()))
                        ),
                        Sequence(
                            "-= ",
                            AssignmentExpression(typedefNames),
                            push(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop()))
                        ),
                        Sequence(
                            "<<= ",
                            AssignmentExpression(typedefNames),
                            push(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop()))
                        ),
                        Sequence(
                            ">>= ",
                            AssignmentExpression(typedefNames),
                            push(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop()))
                        ),
                        Sequence(
                            "&= ",
                            AssignmentExpression(typedefNames),
                            push(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop()))
                        ),
                        Sequence(
                            "^= ",
                            AssignmentExpression(typedefNames),
                            push(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop()))
                        ),
                        Sequence(
                            "|= ",
                            AssignmentExpression(typedefNames),
                            push(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop()))
                        )
                    )
                )
            );
        }

        @Cached
        public Rule Expression(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> expr = new Var<>();
            return Sequence(
                AssignmentExpression(typedefNames),
                sourceLocation.set(getSourceLocation()),
                expr.set((SourceNode) pop()),
                ZeroOrMore(
                    ", ",
                    AssignmentExpression(typedefNames),
                    expr.set(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop()))
                ),
                push(expr.get())
            );
        }

        @Cached
        public Rule Expression(final Var<SourceNode> expr, final Var<TypedefNames> typedefNames) {
            return Sequence(
                Expression(typedefNames),
                expr.set((SourceNode) pop())
            );
        }

        @Cached
        public Rule ConstantExpression(final Var<TypedefNames> typedefNames) {
            return Sequence(
                ConditionalExpression(typedefNames),
                drop(),
                push(new SourceNode(getSourceLocation(), (SourceNode) pop()))
            );
        }

        @Cached
        public Rule Declaration(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> declarationSpecifiers = new Var<>();
            final Var<SourceNode> initDeclaratorList = new Var<>();
            final Var<Boolean> isTypedef = new Var<>();
            return Sequence(
                DeclarationSpecifiers(typedefNames),
                declarationSpecifiers.set((SourceNode) pop()),
                isTypedef.set(declarationSpecifiers.get().hasStorageClassSpecifier(SourceNode.class)),
                sourceLocation.set(getSourceLocation()),
                Optional(
                    InitDeclaratorList(isTypedef, typedefNames),
                    initDeclaratorList.set((SourceNode) pop())
                ),
                "; ",
                push(new SourceNode(sourceLocation.get(), declarationSpecifiers.get(), initDeclaratorList.isSet() ? initDeclaratorList.get() : new SourceNode(getSourceLocation())))
            );
        }

        @Cached
        public Rule Declaration2(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> declarationSpecifiers = new Var<>(new SourceNode((SourceLocation) null));
            final Var<Boolean> isTypedef = new Var<>();
            return FirstOf(
                Declaration(typedefNames),
                Sequence(
                    ZeroOrMore(
                        DeclarationSpecifiersWithoutTypeSpecifiers(declarationSpecifiers, typedefNames)
                    ),
                    isTypedef.set(declarationSpecifiers.get().hasStorageClassSpecifier(SourceNode.class)),
                    InitDeclaratorList(isTypedef, typedefNames),
                    sourceLocation.set(getSourceLocation()),
                    "; ",
                    push(new SourceNode(sourceLocation.get(), new SourceNode(sourceLocation.get()), (SourceNode) pop()))
                )
            );
        }

        @Cached
        public Rule Declaration3(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> declarationSpecifiers = new Var<>(new SourceNode((SourceLocation) null));
            final Var<Boolean> isTypedef = new Var<>();
            return FirstOf(
                Declaration(typedefNames),
                Sequence(
                    OneOrMore(
                        DeclarationSpecifiersWithoutTypeSpecifiers(declarationSpecifiers, typedefNames)
                    ),
                    isTypedef.set(declarationSpecifiers.get().hasStorageClassSpecifier(SourceNode.class)),
                    InitDeclaratorList(isTypedef, typedefNames),
                    sourceLocation.set(getSourceLocation()),
                    "; ",
                    push(new SourceNode(sourceLocation.get(), new SourceNode(sourceLocation.get()), (SourceNode) pop()))
                )
            );
        }

        @Cached
        public Rule DeclarationSpecifiers(final Var<TypedefNames> typedefNames) {
            final Var<SourceNode> declarationSpecifiers = new Var<>();
            return Sequence(
                declarationSpecifiers.set(new SourceNode((SourceLocation) null)),
                ZeroOrMore(
                    DeclarationSpecifiersWithoutTypeSpecifiers(declarationSpecifiers, typedefNames)
                ),
                TypeSpecifier(typedefNames),
                declarationSpecifiers.set(new SourceNode(getSourceLocation(), declarationSpecifiers.get(), (SourceNode) pop())),
                ZeroOrMore(
                    FirstOf(
                        DeclarationSpecifiersWithoutTypeSpecifiers(declarationSpecifiers, typedefNames),
                        Sequence(
                            TypeSpecifierWithoutTypedefName(typedefNames),
                            declarationSpecifiers.set(new SourceNode(getSourceLocation(), declarationSpecifiers.get(), (SourceNode) pop()))
                        )
                    )
                ),
                push(declarationSpecifiers.get())
            );
        }

        @Cached
        public Rule DeclarationSpecifiersWithoutTypeSpecifiers(final Var<SourceNode> declarationSpecifiers, final Var<TypedefNames> typedefNames) {
            return Sequence(
                FirstOf(
                    StorageClassSpecifier(),
                    TypeQualifier(),
                    FunctionSpecifier(),
                    AttributeSpecifier(typedefNames)
                ),
                declarationSpecifiers.set(new SourceNode(getSourceLocation(), declarationSpecifiers.get(), (SourceNode) pop()))
            );
        }

        @Cached
        public Rule InitDeclaratorList(final Var<Boolean> isTypedef, final Var<TypedefNames> typedefNames) {
            final Var<SourceNode> initDeclaratorList = new Var<>();
            return Sequence(
                InitDeclarator(isTypedef, typedefNames),
                initDeclaratorList.set(new SourceNode(getSourceLocation(), (SourceNode) pop())),
                ZeroOrMore(
                    ", ",
                    InitDeclarator(isTypedef, typedefNames),
                    initDeclaratorList.set(new SourceNode(getSourceLocation(), initDeclaratorList.get(), (SourceNode) pop()))
                ),
                push(initDeclaratorList.get())
            );
        }

        @Cached
        public Rule InitDeclarator(final Var<Boolean> isTypedef, final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> declarator = new Var<>();
            final Var<SourceNode> expr = new Var<>();
            return Sequence(
                Declarator(typedefNames),
                sourceLocation.set(getSourceLocation()),
                declarator.set((SourceNode) pop()),
                !isTypedef.get() || defineTypedefName(typedefNames, declarator.get().getDirectDeclarator().getDeclaredIdentifier()),
                Optional(
                    AttributeSpecifierList(typedefNames)
                ),
                Optional(
                    "= ",
                    Initializer(typedefNames),
                    expr.set((SourceNode) pop())
                ),
                push(expr.isSet() ? new SourceNode(sourceLocation.get(), declarator.get(), expr.get()) : new SourceNode(sourceLocation.get(), declarator.get()))
            );
        }

        public Rule StorageClassSpecifier() {
            return FirstOf(
                Sequence(Keyword("typedef"), push(new SourceNode(getSourceLocation()))),
                Sequence(Keyword("extern"), push(new SourceNode(getSourceLocation()))),
                Sequence(Keyword("static"), push(new SourceNode(getSourceLocation()))),
                Sequence(Keyword("auto"), push(new SourceNode(getSourceLocation()))),
                Sequence(Keyword("register"), push(new SourceNode(getSourceLocation())))
            );
        }

        @Cached
        public Rule TypeSpecifier(final Var<TypedefNames> typedefNames) {
            return FirstOf(
                TypeSpecifierWithoutTypedefName(typedefNames),
                TypedefName(typedefNames),
                TypeofSpecifier(typedefNames)
            );
        }

        @Cached
        public Rule TypeofSpecifier(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            return FirstOf(
                Sequence(
                    Keyword("typeof"),
                    sourceLocation.set(getSourceLocation()),
                    UnaryExpression(typedefNames),
                    push(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                ),
                Sequence(
                    Keyword("typeof"),
                    sourceLocation.set(getSourceLocation()),
                    "( ",
                    TypeName(typedefNames),
                    ") ",
                    push(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                )
            );
        }

        @Cached
        public Rule TypeSpecifierWithoutTypedefName(final Var<TypedefNames> typedefNames) {
            return FirstOf(
                Sequence(Keyword("void"), push(new SourceNode(getSourceLocation()))),
                Sequence(Keyword("char"), push(new SourceNode(getSourceLocation()))),
                Sequence(Keyword("short"), push(new SourceNode(getSourceLocation()))),
                Sequence(Keyword("int"), push(new SourceNode(getSourceLocation()))),
                Sequence(Keyword("long"), push(new SourceNode(getSourceLocation()))),
                Sequence(Keyword("float"), push(new SourceNode(getSourceLocation()))),
                Sequence(Keyword("double"), push(new SourceNode(getSourceLocation()))),
                Sequence(Keyword("signed"), push(new SourceNode(getSourceLocation()))),
                Sequence(Keyword("unsigned"), push(new SourceNode(getSourceLocation()))),
                Sequence(Keyword("_Bool"), push(new SourceNode(getSourceLocation()))),
                Sequence(Keyword("_Complex"), push(new SourceNode(getSourceLocation()))),
                Sequence(Keyword("_Imaginary"), push(new SourceNode(getSourceLocation()))),
                StructOrUnionSpecifier(typedefNames),
                EnumSpecifier(typedefNames)
            );
        }

        @Cached
        public Rule StructOrUnionSpecifier(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<Identifier> identifier = new Var<>();
            final Var<Boolean> isStruct = new Var<>();
            return FirstOf(
                Sequence(
                    StructOrUnion(isStruct),
                    sourceLocation.set(getSourceLocation()),
                    Optional(
                        AttributeSpecifierList(typedefNames)
                    ),
                    Optional(
                        Identifier(), identifier.set((Identifier) pop()), isNonKeyword(identifier.get())
                    ),
                    "{ ",
                    StructDeclarationList(typedefNames),
                    "} ",
                    Optional(
                        AttributeSpecifierList(typedefNames)
                    ),
                    push(identifier.isSet()
                        ? isStruct.get() ? new SourceNode(sourceLocation.get(), identifier.get(), (SourceNode) pop()) : new SourceNode(sourceLocation.get(), identifier.get(), (SourceNode) pop())
                        : isStruct.get() ? new SourceNode(sourceLocation.get(), (SourceNode) pop()) : new SourceNode(sourceLocation.get(), (SourceNode) pop())
                    )
                ),
                Sequence(
                    StructOrUnion(isStruct),
                    sourceLocation.set(getSourceLocation()),
                    Optional(
                        AttributeSpecifierList(typedefNames)
                    ),
                    Identifier(), identifier.set((Identifier) pop()), isNonKeyword(identifier.get()),
                    push(isStruct.get() ? new SourceNode(sourceLocation.get(), identifier.get()) : new SourceNode(sourceLocation.get(), identifier.get()))
                )
            );
        }

        @Cached
        public Rule StructOrUnion(final Var<Boolean> isStruct) {
            return FirstOf(
                Sequence(Keyword("struct"), isStruct.set(true)),
                Sequence(Keyword("union"), isStruct.set(false))
            );
        }

        public Rule StructDeclarationList(final Var<TypedefNames> typedefNames) {
            final Var<SourceNode> structDeclarationList = new Var<>();
            return Sequence(
                Optional(
                    StructDeclaration(typedefNames),
                    structDeclarationList.set(new SourceNode(getSourceLocation(), (SourceNode) pop())),
                    ZeroOrMore(
                        StructDeclaration(typedefNames),
                        structDeclarationList.set(new SourceNode(structDeclarationList.get(), (SourceNode) pop()))
                    )
                ),
                push(structDeclarationList.isSet() ? structDeclarationList.get() : new SourceNode(getSourceLocation()))
            );
        }

        public Rule StructDeclaration(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> structDeclaratorList = new Var<>();
            return Sequence(
                SpecifierQualifierList(typedefNames),
                sourceLocation.set(getSourceLocation()),
                Optional(
                    StructDeclaratorList(typedefNames),
                    structDeclaratorList.set((SourceNode) pop())
                ),
                "; ",
                push(new SourceNode(sourceLocation.get(), (SourceNode) pop(), structDeclaratorList.isSet() ? structDeclaratorList.get() : new SourceNode(sourceLocation.get())))
            );
        }

        public Rule SpecifierQualifierList(final Var<TypedefNames> typedefNames) {
            final Var<SourceNode> specifierQualifierList = new Var<>();
            return Sequence(
                FirstOf(
                    Sequence(
                        TypeSpecifier(typedefNames),
                        specifierQualifierList.set(new SourceNode(getSourceLocation(), (SourceNode) pop()))
                    ),
                    Sequence(
                        TypeQualifier(),
                        specifierQualifierList.set(new SourceNode(getSourceLocation(), (SourceNode) pop()))
                    )
                ),
                ZeroOrMore(
                    FirstOf(
                        Sequence(
                            TypeSpecifier(typedefNames),
                            specifierQualifierList.set(new SourceNode(specifierQualifierList.get(), (SourceNode) pop()))
                        ),
                        Sequence(
                            TypeQualifier(),
                            specifierQualifierList.set(new SourceNode(specifierQualifierList.get(), (SourceNode) pop()))
                        )
                    )
                ),
                push(specifierQualifierList.get())
            );
        }

        public Rule StructDeclaratorList(final Var<TypedefNames> typedefNames) {
            final Var<SourceNode> structDeclaratorList = new Var<>();
            return Sequence(
                StructDeclarator(typedefNames),
                structDeclaratorList.set(new SourceNode(getSourceLocation(), (SourceNode) pop())),
                ZeroOrMore(
                    ", ",
                    StructDeclarator(typedefNames),
                    structDeclaratorList.set(new SourceNode(structDeclaratorList.get(), (SourceNode) pop()))
                ),
                push(structDeclaratorList.get())
            );
        }

        public Rule StructDeclarator(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> declarator = new Var<>();
            return FirstOf(
                Sequence(
                    Optional(
                        Declarator(typedefNames),
                        declarator.set((SourceNode) pop())
                    ),
                    sourceLocation.set(getSourceLocation()),
                    ": ",
                    sourceLocation.isSet() || sourceLocation.set(getSourceLocation()),
                    ConstantExpression(typedefNames),
                    push(declarator.isSet() ? new SourceNode(sourceLocation.get(), declarator.get(), (SourceNode) pop()) : new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                ),
                Sequence(
                    Declarator(typedefNames),
                    push(new SourceNode(getSourceLocation(), (SourceNode) pop()))
                )
            );
        }

        @Cached
        public Rule EnumSpecifier(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<Identifier> identifier = new Var<>();
            return FirstOf(
                Sequence(
                    Keyword("enum"),
                    sourceLocation.set(getSourceLocation()),
                    Optional(
                        AttributeSpecifierList(typedefNames)
                    ),
                    Optional(
                        Identifier(), identifier.set((Identifier) pop()), isNonKeyword(identifier.get())
                    ),
                    "{ ",
                    EnumeratorList(typedefNames),
                    Optional(", "),
                    "} ",
                    Optional(
                        AttributeSpecifierList(typedefNames)
                    ),
                    push(identifier.isSet() ? new SourceNode(sourceLocation.get(), identifier.get(), (SourceNode) pop()) : new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                ),
                Sequence(
                    Keyword("enum"),
                    sourceLocation.set(getSourceLocation()),
                    Optional(
                        AttributeSpecifierList(typedefNames)
                    ),
                    Identifier(), identifier.set((Identifier) pop()), isNonKeyword(identifier.get()),
                    push(new SourceNode(sourceLocation.get(), identifier.get()))
                )
            );
        }

        @Cached
        public Rule EnumeratorList(final Var<TypedefNames> typedefNames) {
            final Var<SourceNode> enumerator = new Var<>();
            return Sequence(
                Enumerator(typedefNames),
                push(new SourceNode(getSourceLocation(), (SourceNode) pop())),
                ZeroOrMore(
                    ", ",
                    Enumerator(typedefNames),
                    enumerator.set((SourceNode) pop()),
                    push(new SourceNode((SourceNode) pop(), enumerator.get()))
                )
            );
        }

        @Cached
        public Rule Enumerator(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> constantExpression = new Var<>();
            final Var<Identifier> identifier = new Var<>();
            return Sequence(
                EnumerationConstant(),
                identifier.set(((SourceNode) pop()).getIdentifier()),
                sourceLocation.set(getSourceLocation()),
                Optional(
                    "= ",
                    ConstantExpression(typedefNames),
                    constantExpression.set((SourceNode) pop())
                ),
                push(constantExpression.isSet() ? new SourceNode(sourceLocation.get(), identifier.get(), constantExpression.get()) : new SourceNode(sourceLocation.get(), identifier.get()))
            );
        }

        public Rule TypeQualifier() {
            return FirstOf(
                Sequence(Keyword("const"), push(new SourceNode(getSourceLocation()))),
                Sequence(Keyword("restrict"), push(new SourceNode(getSourceLocation()))),
                Sequence(Keyword("volatile"), push(new SourceNode(getSourceLocation())))
            );
        }

        public Rule FunctionSpecifier() {
            return Sequence(Keyword("inline"), push(new SourceNode(getSourceLocation())));
        }

        @Cached
        public Rule Declarator(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> pointer = new Var<>();
            final Var<SourceNode> directDeclarator = new Var<>();
            final Var<SourceNode> asm = new Var<>();
            return Sequence(
                Optional(
                    Pointer(typedefNames),
                    sourceLocation.set(getSourceLocation()),
                    pointer.set((SourceNode) pop())
                ),
                DirectDeclarator(typedefNames),
                sourceLocation.isSet() || sourceLocation.set(getSourceLocation()),
                directDeclarator.set((SourceNode) pop()),
                Optional(
                    Keyword("asm"),
                    "( ",
                    StringLiteral(),
                    asm.set((SourceNode) pop()),
                    ") "
                ),
                push(new SourceNode(sourceLocation.get(), pointer.get(), directDeclarator.get(), asm.get()))
            );
        }

        @Cached
        public Rule DirectDeclarator(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceLocation> sourceLocation2 = new Var<>();
            final Var<Identifier> identifier = new Var<>();
            final Var<SourceNode> directDeclarator = new Var<>();
            return Sequence(
                FirstOf(
                    Sequence(
                        Identifier(), identifier.set((Identifier) pop()),
                        sourceLocation.set(getSourceLocation()),
                        directDeclarator.set(new SourceNode(sourceLocation.get(), identifier.get()))
                    ),
                    Sequence(
                        "( ",
                        sourceLocation.set(getSourceLocation()),
                        Declarator(typedefNames),
                        ") ",
                        directDeclarator.set(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                    )
                ),
                ZeroOrMore(
                    FirstOf(
                        Sequence(
                            "[ ",
                            Optional(
                                TypeQualifierList(typedefNames)
                            ),
                            AssignmentExpression(typedefNames),
                            "] ",
                            directDeclarator.set(new SourceNode(sourceLocation.get(), directDeclarator.get(), (SourceNode) pop()))
                        ),
                        Sequence(
                            "[ ",
                            Optional(
                                TypeQualifierList(typedefNames)
                            ),
                            "] ",
                            directDeclarator.set(new SourceNode(sourceLocation.get(), directDeclarator.get()))
                        ),
                        Sequence(
                            "[ ",
                            Keyword("static"),
                            Optional(
                                TypeQualifierList(typedefNames)
                            ),
                            AssignmentExpression(typedefNames),
                            "] ",
                            directDeclarator.set(new SourceNode(sourceLocation.get(), directDeclarator.get(), (SourceNode) pop()))
                        ),
                        Sequence(
                            "[ ",
                            TypeQualifierList(typedefNames),
                            Keyword("static"),
                            AssignmentExpression(typedefNames),
                            "] ",
                            directDeclarator.set(new SourceNode(sourceLocation.get(), directDeclarator.get(), (SourceNode) pop()))
                        ),
                        Sequence(
                            "[ ",
                            Optional(
                                TypeQualifierList(typedefNames)
                            ),
                            "* ",
                            "] ",
                            directDeclarator.set(new SourceNode(sourceLocation.get(), directDeclarator.get()))
                        ),
                        Sequence(
                            "( ",
                            ParameterTypeList(typedefNames),
                            ") ",
                            directDeclarator.set(new SourceNode(sourceLocation.get(), directDeclarator.get(), (SourceNode) pop()))
                        ),
                        Sequence(
                            "( ",
                            IdentifierList(),
                            ") ",
                            directDeclarator.set(new SourceNode(sourceLocation.get(), directDeclarator.get()))
                        ),
                        Sequence(
                            "( ",
                            sourceLocation2.set(getSourceLocation()),
                            ") ",
                            directDeclarator.set(new SourceNode(sourceLocation.get(), sourceLocation2.get(), directDeclarator.get()))
                        )
                    )
                ),
                push(directDeclarator.get())
            );
        }

        public Rule Pointer(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            return Sequence(
                "* ",
                sourceLocation.set(getSourceLocation()),
                Optional(
                    TypeQualifierList(typedefNames)
                ),
                push(new SourceNode(getSourceLocation())),
                ZeroOrMore(
                    "* ",
                    sourceLocation.set(getSourceLocation()),
                    Optional(
                        TypeQualifierList(typedefNames)
                    ),
                    push(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                )
            );
        }

        @Cached
        public Rule TypeQualifierList(final Var<TypedefNames> typedefNames) {
            return OneOrMore(
                FirstOf(
                    Sequence(
                        TypeQualifier(),
                        drop()
                    ),
                    AttributeSpecifierList(typedefNames)
                )
            );
        }

        @Cached
        public Rule ParameterTypeList(final Var<TypedefNames> typedefNames) {
            return Sequence(
                ParameterList(typedefNames),
                Optional(
                    ", ",
                    "... ",
                    push(new SourceNode((SourceNode) pop()))
                )
            );
        }

        @Cached
        public Rule ParameterList(final Var<TypedefNames> typedefNames) {
            final Var<SourceNode> parameterDeclaration = new Var<>();
            return Sequence(
                ParameterDeclaration(typedefNames),
                push(new SourceNode(getSourceLocation(), (SourceNode) pop())),
                ZeroOrMore(
                    ", ",
                    ParameterDeclaration(typedefNames),
                    parameterDeclaration.set((SourceNode) pop()),
                    push(new SourceNode((SourceNode) pop(), parameterDeclaration.get()))
                )
            );
        }

        @Cached
        public Rule ParameterDeclaration(final Var<TypedefNames> typedefNames) {
            final Var<SourceNode> declarationSpecifiers = new Var<>();
            return Sequence(
                DeclarationSpecifiers(typedefNames),
                declarationSpecifiers.set((SourceNode) pop()),
                FirstOf(
                    Sequence(
                        Declarator(typedefNames),
                        Optional(
                            AttributeSpecifierList(typedefNames)
                        ),
                        push(new SourceNode(getSourceLocation(), declarationSpecifiers.get(), (SourceNode) pop()))
                    ),
                    Sequence(
                        AbstractDeclarator(typedefNames),
                        push(new SourceNode(getSourceLocation(), declarationSpecifiers.get(), (SourceNode) pop()))
                    ),
                    Sequence(
                        EMPTY,
                        push(new SourceNode(getSourceLocation(), declarationSpecifiers.get()))
                    )
                )
            );
        }

        public Rule IdentifierList() {
            final Var<Identifier> identifier = new Var<>();
            return Sequence(
                Identifier(), identifier.set((Identifier) pop()), isNonKeyword(identifier.get()),
                ZeroOrMore(
                    ", ",
                    Identifier(), identifier.set((Identifier) pop()), isNonKeyword(identifier.get())
                )
            );
        }

        @Cached
        public Rule TypeName(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> abstractDeclarator = new Var<>();
            return Sequence(
                SpecifierQualifierList(typedefNames),
                sourceLocation.set(getSourceLocation()),
                Optional(
                    AbstractDeclarator(typedefNames),
                    abstractDeclarator.set((SourceNode) pop())
                ),
                push(new SourceNode(sourceLocation.get(), (SourceNode) pop(), abstractDeclarator.get()))
            );
        }

        @Cached
        public Rule AbstractDeclarator(final Var<TypedefNames> typedefNames) {
            final Var<SourceNode> pointer = new Var<>();
            return FirstOf(
                Sequence(
                    Pointer(typedefNames),
                    pointer.set((SourceNode) pop()),
                    DirectAbstractDeclarator(typedefNames),
                    push(new SourceNode(pointer.get().getSourceLocation(), pointer.get(), (SourceNode) pop()))
                ),
                DirectAbstractDeclarator(typedefNames),
                Sequence(
                    Pointer(typedefNames),
                    pointer.set((SourceNode) pop()),
                    push(new SourceNode(pointer.get().getSourceLocation(), pointer.get()))
                )
            );
        }

        @Cached
        public Rule DirectAbstractDeclarator(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> parameterList = new Var<>();
            final Var<SourceNode> expr = new Var<>();
            return Sequence(
                FirstOf(
                    Sequence(
                        "( ",
                        sourceLocation.set(getSourceLocation()),
                        ") ",
                        push(new SourceNode(sourceLocation.get(), new SourceNode(getSourceLocation())))
                    ),
                    Sequence(
                        "(",
                        sourceLocation.set(getSourceLocation()),
                        AbstractDeclarator(typedefNames),
                        ") "
                    ),
                    Sequence(
                        "( ",
                        sourceLocation.set(getSourceLocation()),
                        ParameterTypeList(typedefNames),
                        ") ",
                        push(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                    ),
                    Sequence(
                        "[ ",
                        sourceLocation.set(getSourceLocation()),
                        "] ",
                        push(new SourceNode(sourceLocation.get()))
                    ),
                    Sequence(
                        "[ ",
                        sourceLocation.set(getSourceLocation()),
                        "* ",
                        "] ",
                        push(new SourceNode(sourceLocation.get()))
                    ),
                    Sequence(
                        "[ ",
                        sourceLocation.set(getSourceLocation()),
                        AssignmentExpression(typedefNames),
                        "] ",
                        push(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                    )
                ),
                ZeroOrMore(
                    FirstOf(
                        Sequence(
                            "( ",
                            sourceLocation.set(getSourceLocation()),
                            ") ",
                            push(new SourceNode((SourceNode) pop(), new SourceNode(getSourceLocation())))
                        ),
                        Sequence(
                            "( ",
                            sourceLocation.set(getSourceLocation()),
                            ParameterTypeList(typedefNames),
                            ") ",
                            parameterList.set((SourceNode) pop()),
                            push(new SourceNode((SourceNode) pop(), parameterList.get()))
                        ),
                        Sequence(
                            "[ ",
                            sourceLocation.set(getSourceLocation()),
                            "] ",
                            push(new SourceNode((SourceNode) pop()))
                        ),
                        Sequence(
                            "[ ",
                            sourceLocation.set(getSourceLocation()),
                            "* ",
                            "] ",
                            push(new SourceNode((SourceNode) pop()))
                        ),
                        Sequence(
                            "[ ",
                            sourceLocation.set(getSourceLocation()),
                            AssignmentExpression(typedefNames),
                            expr.set((SourceNode) pop()),
                            "] ",
                            push(new SourceNode((SourceNode) pop(), expr.get()))
                        )
                    )
                )
            );
        }

        @Cached
        public Rule TypedefName(final Var<TypedefNames> typedefNames) {
            final Var<Identifier> identifier = new Var<>();
            return Sequence(
                Identifier(), identifier.set((Identifier) pop()), isTypedefName(typedefNames, identifier.get()),
                push(new SourceNode(getSourceLocation(), identifier.get()))
            );
        }

        @Cached
        public Rule Initializer(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            return FirstOf(
                AssignmentExpression(typedefNames),
                Sequence(
                    "{ ",
                    sourceLocation.set(getSourceLocation()),
                    InitializerList(typedefNames),
                    Optional(
                        ", "
                    ),
                    "} ",
                    push(new SourceNode(sourceLocation.get(), new SourceNode(sourceLocation.get(), BigInteger.ZERO, new SourceNode(sourceLocation.get()))))
                )
            );
        }

        public Rule InitializerList(final Var<TypedefNames> typedefNames) {
            return Sequence(
                Optional(
                    Designation(typedefNames)
                ),
                Initializer(typedefNames),
                drop(),
                ZeroOrMore(
                    ", ",
                    Optional(
                        Designation(typedefNames)
                    ),
                    Initializer(typedefNames),
                    drop()
                )
            );
        }

        public Rule Designation(final Var<TypedefNames> typedefNames) {
            return Sequence(
                DesignatorList(typedefNames),
                "= "
            );
        }

        public Rule DesignatorList(final Var<TypedefNames> typedefNames) {
            return OneOrMore(
                Designator(typedefNames)
            );
        }

        public Rule Designator(final Var<TypedefNames> typedefNames) {
            final Var<Identifier> identifier = new Var<>();
            return FirstOf(
                Sequence(
                    "[ ",
                    ConstantExpression(typedefNames),
                    drop(),
                    "] "
                ),
                Sequence(
                    ". ",
                    Identifier(), identifier.set((Identifier) pop()), isNonKeyword(identifier.get())
                )
            );
        }

        @Cached
        public Rule Statement(final Var<TypedefNames> typedefNames) {
            return FirstOf(
                ExpressionStatement(typedefNames),
                CompoundStatement(typedefNames),
                SelectionStatement(typedefNames),
                IterationStatement(typedefNames),
                JumpStatement(typedefNames),
                LabeledStatement(typedefNames),
                AsmStatement(typedefNames)
            );
        }

        @Cached
        public Rule LabeledStatement(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<Identifier> identifier = new Var<>();
            final Var<SourceNode> constantExpression = new Var<>();
            return FirstOf(
                Sequence(
                    Identifier(), identifier.set((Identifier) pop()), isNonKeyword(identifier.get()),
                    sourceLocation.set(getSourceLocation()),
                    ": ",
                    Optional(
                        AttributeSpecifierList(typedefNames)
                    ),
                    Statement(typedefNames),
                    push(new SourceNode(sourceLocation.get(), identifier.get(), (SourceNode) pop()))
                ),
                Sequence(
                    Keyword("case"),
                    sourceLocation.set(getSourceLocation()),
                    ConstantExpression(typedefNames),
                    constantExpression.set((SourceNode) pop()),
                    ": ",
                    Statement(typedefNames),
                    push(new SourceNode(sourceLocation.get(), constantExpression.get(), (SourceNode) pop()))
                ),
                Sequence(
                    Keyword("default"),
                    sourceLocation.set(getSourceLocation()),
                    ": ",
                    Statement(typedefNames),
                    push(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                )
            );
        }

        @Cached
        public Rule CompoundStatement(final Var<TypedefNames> typedefNames) {
            final Var<SourceNode> compoundStatement = new Var<>();
            return Sequence(
                "{ ",
                compoundStatement.set(new SourceNode(getSourceLocation())),
                ZeroOrMore(
                    BlockItem(compoundStatement, typedefNames)
                ),
                "} ",
                push(compoundStatement.get())
            );
        }

        @Cached
        public Rule BlockItem(final Var<SourceNode> compoundStatement, final Var<TypedefNames> typedefNames) {
            final Var<SourceNode> tmpCompoundStatement = new Var<>();
            return FirstOf(
                Sequence(
                    Declaration3(typedefNames),
                    compoundStatement.set(new SourceNode(compoundStatement.get(), (SourceNode) pop()))
                ),
                Sequence(
                    tmpCompoundStatement.set(compoundStatement.get()),
                    Statement(typedefNames),
                    compoundStatement.set(new SourceNode(tmpCompoundStatement.get(), (SourceNode) pop()))
                )
            );
        }

        @Cached
        public Rule ExpressionStatement(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            return FirstOf(
                Sequence(
                    "; ",
                    push(new SourceNode(getSourceLocation()))
                ),
                Sequence(
                    Expression(typedefNames),
                    sourceLocation.set(getSourceLocation()),
                    "; ",
                    push(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                )
            );
        }

        @Cached
        public Rule SelectionStatement(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> expr = new Var<>();
            final Var<SourceNode> statement = new Var<>();
            return FirstOf(
                Sequence(
                    Keyword("if"),
                    sourceLocation.set(getSourceLocation()),
                    "( ",
                    Expression(typedefNames),
                    expr.set((SourceNode) pop()),
                    ") ",
                    Statement(typedefNames),
                    statement.set((SourceNode) pop()),
                    FirstOf(
                        Sequence(
                            Keyword("else"),
                            Statement(typedefNames),
                            push(new SourceNode(sourceLocation.get(), expr.get(), statement.get(), (SourceNode) pop()))
                        ),
                        push(new SourceNode(sourceLocation.get(), expr.get(), statement.get()))
                    )
                ),
                Sequence(
                    Keyword("switch"),
                    sourceLocation.set(getSourceLocation()),
                    "( ",
                    Expression(typedefNames),
                    expr.set((SourceNode) pop()),
                    ") ",
                    Statement(typedefNames),
                    push(new SourceNode(sourceLocation.get(), expr.get(), (SourceNode) pop()))
                )
            );
        }

        @Cached
        public Rule IterationStatement(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> declaration = new Var<>();
            final Var<SourceNode> initExpr = new Var<>();
            final Var<SourceNode> conditionExpr = new Var<>();
            final Var<SourceNode> stepExpr = new Var<>();
            return FirstOf(
                Sequence(
                    Keyword("while"),
                    sourceLocation.set(getSourceLocation()),
                    "( ",
                    Expression(typedefNames),
                    conditionExpr.set((SourceNode) pop()),
                    ") ",
                    Statement(typedefNames),
                    push(new SourceNode(sourceLocation.get(), conditionExpr.get(), (SourceNode) pop()))
                ),
                Sequence(
                    Keyword("do"),
                    sourceLocation.set(getSourceLocation()),
                    Statement(typedefNames),
                    Keyword("while"),
                    "( ",
                    Expression(typedefNames),
                    conditionExpr.set((SourceNode) pop()),
                    ") ",
                    "; ",
                    push(new SourceNode(sourceLocation.get(), conditionExpr.get(), (SourceNode) pop()))
                ),
                Sequence(
                    Keyword("for"),
                    sourceLocation.set(getSourceLocation()),
                    "( ",
                    Optional(
                        Expression(typedefNames),
                        initExpr.set((SourceNode) pop())
                    ),
                    "; ",
                    Optional(
                        Expression(typedefNames),
                        conditionExpr.set((SourceNode) pop())
                    ),
                    "; ",
                    Optional(
                        Expression(typedefNames),
                        stepExpr.set((SourceNode) pop())
                    ),
                    ") ",
                    Statement(typedefNames),
                    push(new SourceNode(sourceLocation.get(), initExpr.get(), conditionExpr.get(), stepExpr.get(), (SourceNode) pop()))
                ),
                Sequence(
                    Keyword("for"),
                    sourceLocation.set(getSourceLocation()),
                    "( ",
                    Declaration2(typedefNames),
                    Optional(
                        Expression(typedefNames),
                        initExpr.set((SourceNode) pop())
                    ),
                    declaration.set((SourceNode) pop()),
                    "; ",
                    Optional(
                        Expression(typedefNames),
                        conditionExpr.set((SourceNode) pop())
                    ),
                    ") ",
                    Statement(typedefNames),
                    push(new SourceNode(sourceLocation.get(), declaration.get(), initExpr.get(), conditionExpr.get(), (SourceNode) pop()))
                )
            );
        }

        @Cached
        public Rule JumpStatement(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<Identifier> identifier = new Var<>();
            return FirstOf(
                Sequence(
                    Keyword("goto"),
                    sourceLocation.set(getSourceLocation()),
                    Identifier(), identifier.set((Identifier) pop()), isNonKeyword(identifier.get()),
                    "; ",
                    push(new SourceNode(sourceLocation.get(), identifier.get()))
                ),
                Sequence(
                    Keyword("continue"),
                    sourceLocation.set(getSourceLocation()),
                    "; ",
                    push(new SourceNode(sourceLocation.get()))
                ),
                Sequence(
                    Keyword("break"),
                    sourceLocation.set(getSourceLocation()),
                    "; ",
                    push(new SourceNode(sourceLocation.get()))
                ),
                Sequence(
                    Keyword("return"),
                    sourceLocation.set(getSourceLocation()),
                    "; ",
                    push(new SourceNode(sourceLocation.get()))
                ),
                Sequence(
                    Keyword("return"),
                    sourceLocation.set(getSourceLocation()),
                    Expression(typedefNames),
                    "; ",
                    push(new SourceNode(sourceLocation.get(), (SourceNode) pop()))
                )
            );
        }

        public Rule TranslationUnit(final Var<SourceNode> translationUnit) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<TypedefNames> typedefNames = new Var<>(new TypedefNames());
            return Sequence(
                Optional(
                    SourceLine()
                ),
                sourceLocation.set(getSourceLocation()),
                WhiteSpace(),
                translationUnit.set(new SourceNode(sourceLocation.get())),
                ZeroOrMore(
                    ExternalDeclaration(typedefNames),
                    translationUnit.set(new SourceNode(translationUnit.get(), (SourceNode) pop()))
                ),
                EOI
            );
        }

        @Cached
        public Rule ExternalDeclaration(final Var<TypedefNames> typedefNames) {
            return FirstOf(
                FunctionDefinition(typedefNames),
                Declaration2(typedefNames),
                AsmStatement(typedefNames)
            );
        }

        @Cached
        public Rule FunctionDefinition(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<SourceNode> declarationSpecifiers = new Var<>();
            final Var<SourceNode> declarator = new Var<>();
            final Var<SourceNode> declarationList = new Var<>();
            return Sequence(
                Optional(
                    DeclarationSpecifiers(typedefNames),
                    declarationSpecifiers.set((SourceNode) pop())
                ),
                sourceLocation.set(getSourceLocation()),
                declarationSpecifiers.isSet() || declarationSpecifiers.set(new SourceNode(sourceLocation.get())),
                Declarator(typedefNames),
                declarator.set((SourceNode) pop()),
                Optional(
                    DeclarationList(typedefNames),
                    declarationList.set((SourceNode) pop())
                ),
                CompoundStatement(typedefNames),
                push(new SourceNode(sourceLocation.get(), declarationSpecifiers.get(), declarator.get(), declarationList.get(), (SourceNode) pop()))
            );
        }

        @Cached
        public Rule DeclarationList(final Var<TypedefNames> typedefNames) {
            final Var<SourceNode> declarationList = new Var<>();
            return Sequence(
                Declaration(typedefNames),
                declarationList.set(new SourceNode(getSourceLocation(), (SourceNode) pop())),
                ZeroOrMore(
                    Declaration(typedefNames),
                    declarationList.set(new SourceNode(declarationList.get(), (SourceNode) pop()))
                ),
                push(declarationList.get())
            );
        }

        @Override
        protected Rule fromStringLiteral(final String string) {
            return string.endsWith(" ") ? Sequence(String(string.substring(0, string.length() - 1)), WhiteSpace()) : String(string);
        }

        public Rule WhiteSpace() {
            return ZeroOrMore(
                FirstOf(
                    Sequence("\n", SourceLine()),
                    Sequence("\n", PragmaLine()),
                    AnyOf(" \t\f\r\n")
                )
            );
        }

        public Rule SourceLine() {
            final Var<String> file = new Var<>();
            final Var<String> line = new Var<>();
            return Sequence(
                '#',
                ZeroOrMore(
                    Ch(' ')
                ),
                OneOrMore(
                    CharRange('0', '9')
                ),
                line.set(getContext().getMatch()),
                OneOrMore(
                    Ch(' ')
                ),
                '"',
                ZeroOrMore(
                    NoneOf("\n\"")
                ),
                file.set(getContext().getMatch()),
                '"',
                ZeroOrMore(
                    NoneOf("\n")
                ),
                setSourceLocation(file.get(), line.get())
            );
        }

        public Rule PragmaLine() {
            return Sequence(
                '#',
                WhiteSpace(),
                Keyword("pragma"),
                ZeroOrMore(
                    NoneOf("\n")
                )
            );
        }

        public Rule Keyword(final String keyword) {
            return Sequence(
                String(keyword),
                TestNot(
                    IdentifierCharacter()
                ),
                WhiteSpace()
            );
        }

        @Cached
        public Rule AsmStatement(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<Boolean> hasVolatile = new Var<>(false);
            final Var<SourceNode> outputParameters = new Var<>();
            final Var<SourceLocation> outputSourceLocation = new Var<>();
            final Var<SourceNode> inputParameters = new Var<>();
            final Var<SourceLocation> inputSourceLocation = new Var<>();
            final Var<SourceNode> code = new Var<>();
            return Sequence(
                Keyword("asm"),
                sourceLocation.set(getSourceLocation()),
                Optional(
                    Keyword("volatile"),
                    hasVolatile.set(true)
                ),
                "( ",
                StringLiteral(),
                code.set((SourceNode) pop()),
                outputSourceLocation.set(getSourceLocation()),
                inputSourceLocation.set(outputSourceLocation.get()),
                Optional(
                    ": ",
                    outputSourceLocation.set(getSourceLocation()),
                    inputSourceLocation.set(outputSourceLocation.get()),
                    Optional(
                        AsmSpecList(typedefNames),
                        outputParameters.set((SourceNode) pop()),
                        inputSourceLocation.set(getSourceLocation())
                    ),
                    Optional(
                        ": ",
                        inputSourceLocation.set(getSourceLocation()),
                        Optional(
                            AsmSpecList(typedefNames),
                            inputParameters.set((SourceNode) pop())
                        ),
                        Optional(
                            ": ",
                            Optional(
                                StringList()
                            )
                        )
                    )
                ),
                ") ",
                "; ",
                push(new SourceNode(sourceLocation.get(), hasVolatile.get(), code.get(), outputParameters.isSet() ? outputParameters.get() : new SourceNode(outputSourceLocation.get()), inputParameters.isSet() ? inputParameters.get() : new SourceNode(inputSourceLocation.get())))
            );
        }

        @Cached
        public Rule AsmSpecList(final Var<TypedefNames> typedefNames) {
            final Var<SourceNode> asmSpecList = new Var<>();
            return Sequence(
                AsmSpec(typedefNames),
                asmSpecList.set(new SourceNode(getSourceLocation(), (SourceNode) pop())),
                ZeroOrMore(
                    ", ",
                    AsmSpec(typedefNames),
                    asmSpecList.set(new SourceNode(asmSpecList.get(), (SourceNode) pop()))
                ),
                push(asmSpecList.get())
            );
        }

        @Cached
        public Rule AsmSpec(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            final Var<Identifier> identifier = new Var<>();
            return Sequence(
                Optional(
                    "[ ",
                    sourceLocation.set(getSourceLocation()),
                    Identifier(), identifier.set((Identifier) pop()), isNonKeyword(identifier.get()),
                    "] "
                ),
                StringLiteral(),
                sourceLocation.isSet() || sourceLocation.set(getSourceLocation()),
                "( ",
                Expression(typedefNames),
                ") ",
                push(identifier.isSet() ? new SourceNode(sourceLocation.get(), identifier.get(), (SourceNode) pop(1), (SourceNode) pop()) : new SourceNode(sourceLocation.get(), (SourceNode) pop(1), (SourceNode) pop()))
            );
        }

        public Rule StringList() {
            return Sequence(
                StringLiteral(),
                drop(),
                ZeroOrMore(
                    ", ",
                    StringLiteral(),
                    drop()
                )
            );
        }

        public Rule AttributeSpecifierList(final Var<TypedefNames> typedefNames) {
            return OneOrMore(
                AttributeSpecifier(typedefNames),
                drop()
            );
        }

        public Rule AttributeSpecifier(final Var<TypedefNames> typedefNames) {
            final Var<SourceLocation> sourceLocation = new Var<>();
            return Sequence(
                Keyword("__attribute__"),
                sourceLocation.set(getSourceLocation()),
                "( ",
                "( ",
                AttributeList(typedefNames),
                ") ",
                ") ",
                push(new SourceNode(sourceLocation.get()))
            );
        }

        public Rule AttributeList(final Var<TypedefNames> typedefNames) {
            return Sequence(
                Attribute(typedefNames),
                ZeroOrMore(
                    ", ",
                    Attribute(typedefNames)
                )
            );
        }

        public Rule Attribute(final Var<TypedefNames> typedefNames) {
            return Optional(
                Identifier(),
                drop(),
                Optional(
                    "( ",
                    Optional(
                        Expression(typedefNames),
                        drop(),
                        ZeroOrMore(
                            ", ",
                            Expression(typedefNames),
                            drop()
                        )
                    ),
                    ") "
                )
            );
        }

        public static boolean defineTypedefName(final Var<TypedefNames> typedefNames, final Identifier identifier) {
            typedefNames.get().addTypedefName(identifier.getName());
            return true;
        }

        public boolean setSourceLocation(final String file, final String line) {
            int line2;
            try {
                line2 = Integer.parseInt(line);
            } catch (final NumberFormatException ignored) {
                line2 = 1;
            }
            sourceLocations.setSourceLocation(position().line + 1, currentIndex(), file, line2);
            return true;
        }

        public SourceLocation getSourceLocation() {
            final int startIndex = getContext().getStartIndex();
            return sourceLocations.getSourceLocation(getContext().getInputBuffer().getPosition(startIndex), startIndex);
        }

        public static boolean isNonKeyword(final Identifier identifier) {
            return !KEYWORDS.contains(identifier.getName());
        }

        public static boolean isTypedefName(final Var<TypedefNames> typedefNames, final Identifier identifier) {
            return isNonKeyword(identifier) && typedefNames.get().isTypedefName(identifier.getName());
        }

        public static boolean parseIntegerConstant(final Var<BigInteger> result, final String string, final int radix) {
            result.set(new BigInteger(string, radix));
            return true;
        }

    }

    public static class SourceNode {

        private final SourceLocation sourceLocation;

        public SourceNode(final SourceLocation sourceLocation) {
            this.sourceLocation = sourceLocation;
        }

        public SourceNode(final SourceLocation sourceLocation, final BigInteger bigInteger, final SourceNode sourceNode) {
            this.sourceLocation = sourceLocation;
        }

        public SourceNode(final SourceLocation sourceLocation, final String string) {
            this.sourceLocation = sourceLocation;
        }

        public SourceNode(final SourceNode sourceNode, final SourceNode sourceNode2) {
            sourceLocation = sourceNode.sourceLocation;
        }

        public SourceNode(final SourceLocation sourceLocation, final SourceNode sourceNode) {
            this.sourceLocation = sourceLocation;
        }

        public SourceNode(final SourceLocation sourceLocation, final boolean b) {
            this.sourceLocation = sourceLocation;
        }

        public SourceNode(final SourceLocation sourceLocation, final SourceNode sourceNode, final SourceNode sourceNode2) {
            this.sourceLocation = sourceLocation;
        }

        public SourceNode(final SourceLocation sourceLocation, final SourceNode sourceNode, final SourceNode sourceNode2, final SourceNode sourceNode3) {
            this.sourceLocation = sourceLocation;
        }

        public SourceNode(final SourceLocation sourceLocation, final SourceLocation sourceLocation2, final SourceNode sourceNode) {
            this.sourceLocation = sourceLocation;
        }

        public SourceNode(final SourceNode sourceNode) {
            sourceLocation = sourceNode.sourceLocation;
        }

        public SourceNode(final SourceLocation sourceLocation, final Identifier identifier, final SourceNode sourceNode) {
            this.sourceLocation = sourceLocation;
        }

        public SourceNode(final SourceLocation sourceLocation, final SourceNode sourceNode, final SourceNode sourceNode2, final SourceNode sourceNode3, final SourceNode sourceNode4) {
            this.sourceLocation = sourceLocation;
        }

        public SourceNode(final SourceLocation sourceLocation, final Boolean b, final SourceNode sourceNode, final SourceNode sourceNode2, final SourceNode sourceNode3) {
            this.sourceLocation = sourceLocation;
        }

        public SourceLocation getSourceLocation() {
            return sourceLocation;
        }

        public boolean hasStorageClassSpecifier(final Class<?> class_) {
            return false;
        }

        public SourceNode getDirectDeclarator() {
            return this;
        }

        public Identifier getDeclaredIdentifier() {
            return new Identifier(new SourceLocation(null, 0, "", 0, 0), "dummy");
        }

        public Identifier getIdentifier() {
            return new Identifier(new SourceLocation(null, 0, "", 0, 0), "dummy");
        }

    }

    public static class Identifier extends SourceNode {

        private final String name;

        public Identifier(final SourceLocation sourceLocation, final String name) {
            super(sourceLocation);
            this.name = name;
        }

        public String getName() {
            return name;
        }

    }

    public static class SourceLocation {

        private final InputBuffer buffer;

        private final int index;

        private final String file;

        private final int line;

        private final int column;

        public SourceLocation(final InputBuffer buffer, final int index, final String file, final int line, final int column) {
            this.buffer = buffer;
            this.index = index;
            this.file = file;
            this.line = line;
            this.column = column;
        }

        @Override
        public String toString() {
            return file + ":" + line + ":" + column;

        }

    }

    public static class BooleanMarker extends SourceNode {

        public static final BooleanMarker FALSE = new BooleanMarker(false);

        public static final BooleanMarker TRUE = new BooleanMarker(true);

        private final boolean value;

        private BooleanMarker(final boolean value) {
            super(new SourceLocation(null, 0, "", 0, 0));
            this.value = value;
        }

        public boolean getValue() {
            return value;
        }

    }

    public static class SourceLocations {

        public void setSourceLocation(final int line, final int index, final String file, final int line2) {
        }

        public SourceLocation getSourceLocation(final Position position, final int index) {
            return new SourceLocation(null, 0, "", 0, 0);
        }

    }

    public static class TypedefNames {

        private final Collection<String> typedefNames = new HashSet<>();

        public void addTypedefName(final String name) {
            typedefNames.add(name);
        }

        public boolean isTypedefName(final String name) {
            return typedefNames.contains(name);
        }

    }

}
