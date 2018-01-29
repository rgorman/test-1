<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    exclude-result-prefixes="xs"
    version="2.0">
    
    <xsl:output method="xml" indent="yes"/>
    
    
    <xsl:template match="node() | @*">
        <xsl:copy>
            <xsl:apply-templates select="node() | @*"/>
        </xsl:copy>
    </xsl:template>
    
    <xsl:template match="word[not(@lemma)]">
        <word>
            <xsl:attribute name="lemma">NA</xsl:attribute>
            <xsl:copy-of select="@*"/>
        </word>
    </xsl:template>
    
    <xsl:template match="word[not(@postag)]">
        <word>
            <xsl:attribute name="postag">NA</xsl:attribute>
            <xsl:copy-of select="@*"/>
        </word>
    </xsl:template>
    
    <xsl:template match="word[not(@form)]">
        <word>
            <xsl:attribute name="form">NA</xsl:attribute>
            <xsl:copy-of select="@*"/>
        </word>
    </xsl:template>
    
    <xsl:template match="word">
        
        <xsl:variable name="pos" select="substring(./@postag, 1, 1)"/>
        <xsl:variable name="person" select="substring(./@postag, 2, 1)"/>
        <xsl:variable name="number" select="substring(./@postag, 3, 1)"/>
        <xsl:variable name="tense" select="substring(./@postag, 4, 1)"/>
        <xsl:variable name="mood" select="substring(./@postag, 5, 1)"/>
        <xsl:variable name="voice" select="substring(./@postag, 6, 1)"/>
        <xsl:variable name="gender" select="substring(./@postag, 7, 1)"/>
        <xsl:variable name="case" select="substring(./@postag, 8, 1)"/>
        <xsl:variable name="degree" select="substring(./@postag, 9, 1)"/>
        
        <word>
            <xsl:copy-of select="@*"/>
            <xsl:attribute name="self-relation"><xsl:value-of select="./@relation"/></xsl:attribute>
            <xsl:attribute name="self-depdist"><xsl:value-of select="./@DepDist"/></xsl:attribute>
            <xsl:attribute name="self-vertexDegree"><xsl:value-of select="./@Degree"/></xsl:attribute>
            
            
            <xsl:choose>
                <xsl:when test="$pos='d'">
                    <xsl:attribute name="self-morph-pos">adverb</xsl:attribute>
                </xsl:when>
                <xsl:when test="$pos='r'">
                    <xsl:attribute name="self-morph-pos">preposition</xsl:attribute>
                </xsl:when>
                <xsl:when test="$pos='l'">
                    <xsl:attribute name="self-morph-pos">article</xsl:attribute>
                </xsl:when>
                <xsl:when test="$pos='v'">
                    <xsl:attribute name="self-morph-pos">verb</xsl:attribute>
                </xsl:when>
                <xsl:when test="$pos='a'">
                    <xsl:attribute name="self-morph-pos">adjective</xsl:attribute>
                </xsl:when>
                <xsl:when test="$pos='p'">
                    <xsl:attribute name="self-morph-pos">pronoun</xsl:attribute>
                </xsl:when>
                <xsl:when test="$pos='n'">
                    <xsl:attribute name="self-morph-pos">noun</xsl:attribute>
                </xsl:when>
                <xsl:when test="$pos='c'">
                    <xsl:attribute name="self-morph-pos">conjunction</xsl:attribute>
                </xsl:when>
                <xsl:when test="$pos='u'">
                    <xsl:attribute name="self-morph-pos">punctuation</xsl:attribute>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:attribute name="self-morph-pos">NA</xsl:attribute>
                </xsl:otherwise>
            </xsl:choose>
            
            <xsl:choose>
                <xsl:when test="$person='1'">
                    <xsl:attribute name="self-morph-person">firstPerson</xsl:attribute>
                </xsl:when>
                <xsl:when test="$person='2'">
                    <xsl:attribute name="self-morph-person">secondPerson</xsl:attribute>
                </xsl:when>
                <xsl:when test="$person='3'">
                    <xsl:attribute name="self-morph-person">thirdPerson</xsl:attribute>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:attribute name="self-morph-person">NA</xsl:attribute>
                </xsl:otherwise>
            </xsl:choose>
            
            <xsl:choose>
                <xsl:when test="$number='s'">
                    <xsl:attribute name="self-morph-number">singular</xsl:attribute>
                </xsl:when>
                <xsl:when test="$number='p'">
                    <xsl:attribute name="self-morph-number">plural</xsl:attribute>
                </xsl:when>
                <xsl:when test="$number='d'">
                    <xsl:attribute name="self-morph-number">dual</xsl:attribute>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:attribute name="self-morph-number">NA</xsl:attribute>
                </xsl:otherwise>
            </xsl:choose>
            
            <xsl:choose>
                <xsl:when test="$tense='p'">
                    <xsl:attribute name="self-morph-tense">present</xsl:attribute>
                </xsl:when>
                <xsl:when test="$tense='i'">
                    <xsl:attribute name="self-morph-tense">imperfect</xsl:attribute>
                </xsl:when>
                <xsl:when test="$tense='a'">
                    <xsl:attribute name="self-morph-tense">aorist</xsl:attribute>
                </xsl:when>
                <xsl:when test="$tense='r'">
                    <xsl:attribute name="self-morph-tense">perfect</xsl:attribute>
                </xsl:when>
                <xsl:when test="$tense='l'">
                    <xsl:attribute name="self-morph-tense">pluperfect</xsl:attribute>
                </xsl:when>
                <xsl:when test="$tense='f'">
                    <xsl:attribute name="self-morph-tense">future</xsl:attribute>
                </xsl:when>
                <xsl:when test="$tense='t'">
                    <xsl:attribute name="self-morph-tense">futurePerfect</xsl:attribute>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:attribute name="self-morph-tense">NA</xsl:attribute>
                </xsl:otherwise>
            </xsl:choose>
            
            <xsl:choose>
                <xsl:when test="$mood='i'">
                    <xsl:attribute name="self-morph-mood">indicative</xsl:attribute>
                </xsl:when>
                <xsl:when test="$mood='s'">
                    <xsl:attribute name="self-morph-mood">subjunctive</xsl:attribute>
                </xsl:when>
                <xsl:when test="$mood='o'">
                    <xsl:attribute name="self-morph-mood">optative</xsl:attribute>
                </xsl:when>
                <xsl:when test="$mood='n'">
                    <xsl:attribute name="self-morph-mood">infinitive</xsl:attribute>
                </xsl:when>
                <xsl:when test="$mood='p'">
                    <xsl:attribute name="self-morph-mood">participle</xsl:attribute>
                </xsl:when>
                <xsl:when test="$mood='m'">
                    <xsl:attribute name="self-morph-mood">imperative</xsl:attribute>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:attribute name="self-morph-mood">NA</xsl:attribute>
                </xsl:otherwise>
            </xsl:choose>
            
            <xsl:choose>
                <xsl:when test="$voice='a'">
                    <xsl:attribute name="self-morph-voice">active</xsl:attribute>
                </xsl:when>
                <xsl:when test="$voice='p'">
                    <xsl:attribute name="self-morph-voice">passive</xsl:attribute>
                </xsl:when>
                <xsl:when test="$voice='m'">
                    <xsl:attribute name="self-morph-voice">middle</xsl:attribute>
                </xsl:when>
                <xsl:when test="$voice='e'">
                    <xsl:attribute name="self-morph-voice">deponent</xsl:attribute>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:attribute name="self-morph-voice">NA</xsl:attribute>
                </xsl:otherwise>
            </xsl:choose>
            
            <xsl:choose>
                <xsl:when test="$gender='m'">
                    <xsl:attribute name="self-morph-gender">masculine</xsl:attribute>
                </xsl:when>
                <xsl:when test="$gender='f'">
                    <xsl:attribute name="self-morph-gender">feminine</xsl:attribute>
                </xsl:when>
                <xsl:when test="$gender='n'">
                    <xsl:attribute name="self-morph-gender">neuter</xsl:attribute>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:attribute name="self-morph-gender">NA</xsl:attribute>
                </xsl:otherwise>
            </xsl:choose>
            
            <xsl:choose>
                <xsl:when test="$case='n'">
                    <xsl:attribute name="self-morph-case">nominative</xsl:attribute>
                </xsl:when>
                <xsl:when test="$case='g'">
                    <xsl:attribute name="self-morph-case">genitive</xsl:attribute>
                </xsl:when>
                <xsl:when test="$case='d'">
                    <xsl:attribute name="self-morph-case">dative</xsl:attribute>
                </xsl:when>
                <xsl:when test="$case='a'">
                    <xsl:attribute name="self-morph-case">accusative</xsl:attribute>
                </xsl:when>
                <xsl:when test="$case='b'">
                    <xsl:attribute name="self-morph-case">ablative</xsl:attribute>
                </xsl:when>
                <xsl:when test="$case='v'">
                    <xsl:attribute name="self-morph-case">vocative</xsl:attribute>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:attribute name="self-morph-case">NA</xsl:attribute>
                </xsl:otherwise>
            </xsl:choose>
            
            <xsl:choose>
                <xsl:when test="$degree='p'">
                    <xsl:attribute name="self-morph-degree">positive</xsl:attribute>
                </xsl:when>
                <xsl:when test="$degree='c'">
                    <xsl:attribute name="self-morph-degree">comparative</xsl:attribute>
                </xsl:when>
                <xsl:when test="$degree='s'">
                    <xsl:attribute name="self-morph-degree">superlative</xsl:attribute>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:attribute name="self-morph-degree">NA</xsl:attribute>
                </xsl:otherwise>
            </xsl:choose>
            
            <xsl:variable name="parent_id" select="./@head"/>
            
            <xsl:variable name="parent-pos" select="substring(../word[@id = $parent_id]/@postag, 1, 1)"/>
            <xsl:variable name="parent-person" select="substring(../word[@id = $parent_id]/@postag, 2, 1)"/>
            <xsl:variable name="parent-number" select="substring(../word[@id = $parent_id]/@postag, 3, 1)"/>
            <xsl:variable name="parent-tense" select="substring(../word[@id = $parent_id]/@postag, 4, 1)"/>
            <xsl:variable name="parent-mood" select="substring(../word[@id = $parent_id]/@postag, 5, 1)"/>
            <xsl:variable name="parent-voice" select="substring(../word[@id = $parent_id]/@postag, 6, 1)"/>
            <xsl:variable name="parent-gender" select="substring(../word[@id = $parent_id]/@postag, 7, 1)"/>
            <xsl:variable name="parent-case" select="substring(../word[@id = $parent_id]/@postag, 8, 1)"/>
            <xsl:variable name="parent-degree" select="substring(../word[@id = $parent_id]/@postag, 9, 1)"/>
            
            <xsl:choose>
                <xsl:when test="$parent_id = 0">
                    <xsl:attribute name="parent-relation">root</xsl:attribute>
                    <xsl:attribute name="parent-depdist">NULL</xsl:attribute>
                    <xsl:attribute name="parent-vertexDegree">NULL</xsl:attribute>
                    
                    
                    
                    
                    <xsl:attribute name="parent-morph-pos">NULL</xsl:attribute>
                    <xsl:attribute name="parent-morph-person">NULL</xsl:attribute>
                    <xsl:attribute name="parent-morph-number">NULL</xsl:attribute>
                    <xsl:attribute name="parent-morph-tense">NULL</xsl:attribute>
                    <xsl:attribute name="parent-morph-mood">NULL</xsl:attribute>
                    <xsl:attribute name="parent-morph-voice">NULL</xsl:attribute>
                    <xsl:attribute name="parent-morph-gender">NULL</xsl:attribute>
                    <xsl:attribute name="parent-morph-case">NULL</xsl:attribute>
                    <xsl:attribute name="parent-morph-degree">NULL</xsl:attribute>
                    
                </xsl:when>
                <xsl:when test="$parent_id > 0">
                    <xsl:attribute name="parent-relation"><xsl:value-of select="../word[@id = $parent_id]/@relation"/></xsl:attribute>
                    <xsl:attribute name="parent-depdist"><xsl:value-of select="../word[@id = $parent_id]/@DepDist"/></xsl:attribute>
                    <xsl:attribute name="parent-vertexDegree"><xsl:value-of select="../word[@id = $parent_id]/@Degree"/></xsl:attribute>
                                        
                    <xsl:choose>
                        <xsl:when test="$parent-pos='d'">
                            <xsl:attribute name="parent-morph-pos">adverb</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-pos='r'">
                            <xsl:attribute name="parent-morph-pos">preposition</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-pos='l'">
                            <xsl:attribute name="parent-morph-pos">article</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-pos='v'">
                            <xsl:attribute name="parent-morph-pos">verb</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-pos='a'">
                            <xsl:attribute name="parent-morph-pos">adjective</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-pos='p'">
                            <xsl:attribute name="parent-morph-pos">pronoun</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-pos='n'">
                            <xsl:attribute name="parent-morph-pos">noun</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-pos='c'">
                            <xsl:attribute name="parent-morph-pos">conjunction</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-pos='u'">
                            <xsl:attribute name="parent-morph-pos">punctuation</xsl:attribute>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:attribute name="parent-morph-pos">NA</xsl:attribute>
                        </xsl:otherwise>
                    </xsl:choose>
                    
                    
                    <xsl:choose>
                        <xsl:when test="$parent-person='1'">
                            <xsl:attribute name="parent-morph-person">firstPerson</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-person='2'">
                            <xsl:attribute name="parent-morph-person">secondPerson</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-person='3'">
                            <xsl:attribute name="parent-morph-person">thirdPerson</xsl:attribute>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:attribute name="parent-morph-person">NA</xsl:attribute>
                        </xsl:otherwise>
                    </xsl:choose>
                    
                    <xsl:choose>
                        <xsl:when test="$parent-number='s'">
                            <xsl:attribute name="parent-morph-number">singular</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-number='p'">
                            <xsl:attribute name="parent-morph-number">plural</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-number='d'">
                            <xsl:attribute name="parent-morph-number">dual</xsl:attribute>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:attribute name="parent-morph-number">NA</xsl:attribute>
                        </xsl:otherwise>
                    </xsl:choose>
                    
                    <xsl:choose>
                        <xsl:when test="$parent-tense='p'">
                            <xsl:attribute name="parent-morph-tense">present</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-tense='i'">
                            <xsl:attribute name="parent-morph-tense">imperfect</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-tense='a'">
                            <xsl:attribute name="parent-morph-tense">aorist</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-tense='r'">
                            <xsl:attribute name="parent-morph-tense">perfect</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-tense='l'">
                            <xsl:attribute name="parent-morph-tense">pluperfect</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-tense='f'">
                            <xsl:attribute name="parent-morph-tense">future</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-tense='t'">
                            <xsl:attribute name="parent-morph-tense">futurePerfect</xsl:attribute>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:attribute name="parent-morph-tense">NA</xsl:attribute>
                        </xsl:otherwise>
                    </xsl:choose>
                    
                    <xsl:choose>
                        <xsl:when test="$parent-mood='i'">
                            <xsl:attribute name="parent-morph-mood">indicative</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-mood='s'">
                            <xsl:attribute name="parent-morph-mood">subjunctive</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-mood='o'">
                            <xsl:attribute name="parent-morph-mood">optative</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-mood='n'">
                            <xsl:attribute name="parent-morph-mood">infinitive</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-mood='p'">
                            <xsl:attribute name="parent-morph-mood">participle</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-mood='m'">
                            <xsl:attribute name="parent-morph-mood">imperative</xsl:attribute>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:attribute name="parent-morph-mood">NA</xsl:attribute>
                        </xsl:otherwise>
                    </xsl:choose>
                    
                    <xsl:choose>
                        <xsl:when test="$parent-voice='a'">
                            <xsl:attribute name="parent-morph-voice">active</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-voice='p'">
                            <xsl:attribute name="parent-morph-voice">passive</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-voice='m'">
                            <xsl:attribute name="parent-morph-voice">middle</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-voice='e'">
                            <xsl:attribute name="parent-morph-voice">deponent</xsl:attribute>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:attribute name="parent-morph-voice">NA</xsl:attribute>
                        </xsl:otherwise>
                    </xsl:choose>
                    
                    <xsl:choose>
                        <xsl:when test="$parent-gender='m'">
                            <xsl:attribute name="parent-morph-gender">masculine</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-gender='f'">
                            <xsl:attribute name="parent-morph-gender">feminine</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-gender='n'">
                            <xsl:attribute name="parent-morph-gender">neuter</xsl:attribute>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:attribute name="parent-morph-gender">NA</xsl:attribute>
                        </xsl:otherwise>
                    </xsl:choose>
                    
                    <xsl:choose>
                        <xsl:when test="$parent-case='n'">
                            <xsl:attribute name="parent-morph-case">nominative</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-case='g'">
                            <xsl:attribute name="parent-morph-case">genitive</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-case='d'">
                            <xsl:attribute name="parent-morph-case">dative</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-case='a'">
                            <xsl:attribute name="parent-morph-case">accusative</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-case='b'">
                            <xsl:attribute name="parent-morph-case">ablative</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-case='v'">
                            <xsl:attribute name="parent-morph-case">vocative</xsl:attribute>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:attribute name="parent-morph-case">NA</xsl:attribute>
                        </xsl:otherwise>
                    </xsl:choose>
                    
                    <xsl:choose>
                        <xsl:when test="$parent-degree='p'">
                            <xsl:attribute name="parent-morph-degree">positive</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-degree='c'">
                            <xsl:attribute name="parent-morph-degree">comparative</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$parent-degree='s'">
                            <xsl:attribute name="parent-morph-degree">superlative</xsl:attribute>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:attribute name="parent-morph-degree">NA</xsl:attribute>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:when>
                
            </xsl:choose>
           
            
            
            <xsl:variable name="g1-parent_id"
                select="parent::sentence/word[@id = $parent_id]/@head"/>
            
            <xsl:variable name="g1-parent-pos" select="substring(../word[@id = $g1-parent_id]/@postag, 1, 1)"/>
            <xsl:variable name="g1-parent-person" select="substring(../word[@id = $g1-parent_id]/@postag, 2, 1)"/>
            <xsl:variable name="g1-parent-number" select="substring(../word[@id = $g1-parent_id]/@postag, 3, 1)"/>
            <xsl:variable name="g1-parent-tense" select="substring(../word[@id = $g1-parent_id]/@postag, 4, 1)"/>
            <xsl:variable name="g1-parent-mood" select="substring(../word[@id = $g1-parent_id]/@postag, 5, 1)"/>
            <xsl:variable name="g1-parent-voice" select="substring(../word[@id = $g1-parent_id]/@postag, 6, 1)"/>
            <xsl:variable name="g1-parent-gender" select="substring(../word[@id = $g1-parent_id]/@postag, 7, 1)"/>
            <xsl:variable name="g1-parent-case" select="substring(../word[@id = $g1-parent_id]/@postag, 8, 1)"/>
            <xsl:variable name="g1-parent-degree" select="substring(../word[@id = $g1-parent_id]/@postag, 9, 1)"/>
            
            
            
            
            
            
            
                      
             <xsl:choose>
                 <xsl:when test="$g1-parent_id = 0">
                     <xsl:attribute name="g1-parent-relation">root</xsl:attribute>
                     <xsl:attribute name="g1-parent-depdist">NULL</xsl:attribute>
                     <xsl:attribute name="g1-parent-vertexDegree">NULL</xsl:attribute>
                     
                                          
                     <xsl:attribute name="g1-parent-morph-pos">NULL</xsl:attribute>
                     <xsl:attribute name="g1-parent-morph-person">NULL</xsl:attribute>
                     <xsl:attribute name="g1-parent-morph-number">NULL</xsl:attribute>
                     <xsl:attribute name="g1-parent-morph-tense">NULL</xsl:attribute>
                     <xsl:attribute name="g1-parent-morph-mood">NULL</xsl:attribute>
                     <xsl:attribute name="g1-parent-morph-voice">NULL</xsl:attribute>
                     <xsl:attribute name="g1-parent-morph-gender">NULL</xsl:attribute>
                     <xsl:attribute name="g1-parent-morph-case">NULL</xsl:attribute>
                     <xsl:attribute name="g1-parent-morph-degree">NULL</xsl:attribute>
                     
                     
                 </xsl:when>
                 <xsl:when test="$g1-parent_id > 0">
                     <xsl:attribute name="g1-parent-relation"><xsl:value-of select="../word[@id = $g1-parent_id]/@relation"/></xsl:attribute>
                     <xsl:attribute name="g1-parent-depdist"><xsl:value-of select="../word[@id = $g1-parent_id]/@DepDist"/></xsl:attribute>
                     <xsl:attribute name="g1-parent-vertexDegree"><xsl:value-of select="../word[@id = $g1-parent_id]/@Degree"/></xsl:attribute>
                     
                                          
                     <xsl:choose>
                         <xsl:when test="$g1-parent-pos='d'">
                             <xsl:attribute name="g1-parent-morph-pos">adverb</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-pos='r'">
                             <xsl:attribute name="g1-parent-morph-pos">preposition</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-pos='l'">
                             <xsl:attribute name="g1-parent-morph-pos">article</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-pos='v'">
                             <xsl:attribute name="g1-parent-morph-pos">verb</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-pos='a'">
                             <xsl:attribute name="g1-parent-morph-pos">adjective</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-pos='p'">
                             <xsl:attribute name="g1-parent-morph-pos">pronoun</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-pos='n'">
                             <xsl:attribute name="g1-parent-morph-pos">noun</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-pos='c'">
                             <xsl:attribute name="g1-parent-morph-pos">conjunction</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-pos='u'">
                             <xsl:attribute name="g1-parent-morph-pos">punctuation</xsl:attribute>
                         </xsl:when>
                         <xsl:otherwise>
                             <xsl:attribute name="g1-parent-morph-pos">NA</xsl:attribute>
                         </xsl:otherwise>
                     </xsl:choose>
                     
                     
                         
                             <xsl:choose>
                                 <xsl:when test="$g1-parent-person='1'">
                                     <xsl:attribute name="g1-parent-morph-person">firstPerson</xsl:attribute>
                                 </xsl:when>
                                 <xsl:when test="$g1-parent-person='2'">
                                     <xsl:attribute name="g1-parent-morph-person">secondPerson</xsl:attribute>
                                 </xsl:when>
                                 <xsl:when test="$g1-parent-person='3'">
                                     <xsl:attribute name="g1-parent-morph-person">thirdPerson</xsl:attribute>
                                 </xsl:when>
                                 <xsl:otherwise>
                                     <xsl:attribute name="g1-parent-morph-person">NA</xsl:attribute>
                                 
                                 </xsl:otherwise>
                             </xsl:choose>
                     
                     
                     
                             <xsl:choose>
                                 <xsl:when test="$g1-parent-number='s'">
                                     <xsl:attribute name="g1-parent-morph-number">singular</xsl:attribute>
                                 </xsl:when>
                                 <xsl:when test="$g1-parent-number='p'">
                                     <xsl:attribute name="g1-parent-morph-number">plural</xsl:attribute>
                                 </xsl:when>
                                 <xsl:when test="$g1-parent-number='d'">
                                     <xsl:attribute name="g1-parent-morph-number">dual</xsl:attribute>
                                 </xsl:when>
                                 <xsl:otherwise>
                                     <xsl:attribute name="g1-parent-morph-number">NA</xsl:attribute>
                                 </xsl:otherwise>
                             </xsl:choose>
                             
                    
                         
                             <xsl:choose>
                                 <xsl:when test="$g1-parent-tense='p'">
                                     <xsl:attribute name="g1-parent-morph-tense">present</xsl:attribute>
                                 </xsl:when>
                                 <xsl:when test="$g1-parent-tense='i'">
                                     <xsl:attribute name="g1-parent-morph-tense">imperfect</xsl:attribute>
                                 </xsl:when>
                                 <xsl:when test="$g1-parent-tense='a'">
                                     <xsl:attribute name="g1-parent-morph-tense">aorist</xsl:attribute>
                                 </xsl:when>
                                 <xsl:when test="$g1-parent-tense='r'">
                                     <xsl:attribute name="g1-parent-morph-tense">perfect</xsl:attribute>
                                 </xsl:when>
                                 <xsl:when test="$g1-parent-tense='l'">
                                     <xsl:attribute name="g1-parent-morph-tense">pluperfect</xsl:attribute>
                                 </xsl:when>
                                 <xsl:when test="$g1-parent-tense='f'">
                                     <xsl:attribute name="g1-parent-morph-tense">future</xsl:attribute>
                                 </xsl:when>
                                 <xsl:when test="$g1-parent-tense='t'">
                                     <xsl:attribute name="g1-parent-morph-tense">futurePerfect</xsl:attribute>
                                 </xsl:when>
                                 <xsl:otherwise>
                                     <xsl:attribute name="g1-parent-morph-tense">NA</xsl:attribute>
                                 </xsl:otherwise>
                             </xsl:choose>
                     
                     
                     <xsl:choose>
                         <xsl:when test="$g1-parent-mood='i'">
                             <xsl:attribute name="g1-parent-morph-mood">indicative</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-mood='s'">
                             <xsl:attribute name="g1-parent-morph-mood">subjunctive</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-mood='o'">
                             <xsl:attribute name="g1-parent-morph-mood">optative</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-mood='n'">
                             <xsl:attribute name="g1-parent-morph-mood">infinitive</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-mood='p'">
                             <xsl:attribute name="g1-parent-morph-mood">participle</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-mood='m'">
                             <xsl:attribute name="parent-morph-mood">imperative</xsl:attribute>
                         </xsl:when>
                         <xsl:otherwise>
                             <xsl:attribute name="g1-parent-morph-mood">NA</xsl:attribute>
                         </xsl:otherwise>
                     </xsl:choose>
                     
                     <xsl:choose>
                         <xsl:when test="$g1-parent-voice='a'">
                             <xsl:attribute name="g1-parent-morph-voice">active</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-voice='p'">
                             <xsl:attribute name="g1-parent-morph-voice">passive</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-voice='m'">
                             <xsl:attribute name="g1-parent-morph-voice">middle</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-voice='e'">
                             <xsl:attribute name="g1-parent-morph-voice">deponent</xsl:attribute>
                         </xsl:when>
                         <xsl:otherwise>
                             <xsl:attribute name="g1-parent-morph-voice">NA</xsl:attribute>
                         </xsl:otherwise>
                     </xsl:choose>
                     
                     <xsl:choose>
                         <xsl:when test="$g1-parent-gender='m'">
                             <xsl:attribute name="g1-parent-morph-gender">masculine</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-gender='f'">
                             <xsl:attribute name="g1-parent-morph-gender">feminine</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-gender='n'">
                             <xsl:attribute name="g1-parent-morph-gender">neuter</xsl:attribute>
                         </xsl:when>
                         <xsl:otherwise>
                             <xsl:attribute name="g1-parent-morph-gender">NA</xsl:attribute>
                         </xsl:otherwise>
                     </xsl:choose>
                     
                     <xsl:choose>
                         <xsl:when test="$g1-parent-case='n'">
                             <xsl:attribute name="g1-parent-morph-case">nominative</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-case='g'">
                             <xsl:attribute name="g1-parent-morph-case">genitive</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-case='d'">
                             <xsl:attribute name="g1-parent-morph-case">dative</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-case='a'">
                             <xsl:attribute name="g1-parent-morph-case">accusative</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-case='b'">
                             <xsl:attribute name="g1-parent-morph-case">ablative</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-case='v'">
                             <xsl:attribute name="g1-parent-morph-case">vocative</xsl:attribute>
                         </xsl:when>
                         <xsl:otherwise>
                             <xsl:attribute name="g1-parent-morph-case">NA</xsl:attribute>
                         </xsl:otherwise>
                     </xsl:choose>
                     
                     <xsl:choose>
                         <xsl:when test="$g1-parent-degree='p'">
                             <xsl:attribute name="g1-parent-morph-degree">positive</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-degree='c'">
                             <xsl:attribute name="g1-parent-morph-degree">comparative</xsl:attribute>
                         </xsl:when>
                         <xsl:when test="$g1-parent-degree='s'">
                             <xsl:attribute name="g1-parent-morph-degree">superlative</xsl:attribute>
                         </xsl:when>
                         <xsl:otherwise>
                             <xsl:attribute name="g1-parent-morph-degree">NA</xsl:attribute>
                         </xsl:otherwise>
                     </xsl:choose>
                     
                     
                     
                 </xsl:when>
                 <xsl:otherwise>
                     <xsl:attribute name="g1-parent-relation">NULL</xsl:attribute>
                     <xsl:attribute name="g1-parent-depdist">NULL</xsl:attribute>
                     <xsl:attribute name="g1-parent-vertexDegree">NULL</xsl:attribute>
                     
                                          
                     <xsl:attribute name="g1-parent-morph-pos">NULL</xsl:attribute>
                     <xsl:attribute name="g1-parent-morph-person">NULL</xsl:attribute>
                     <xsl:attribute name="g1-parent-morph-number">NULL</xsl:attribute>
                     <xsl:attribute name="g1-parent-morph-tense">NULL</xsl:attribute>
                     <xsl:attribute name="g1-parent-morph-mood">NULL</xsl:attribute>
                     <xsl:attribute name="g1-parent-morph-voice">NULL</xsl:attribute>
                     <xsl:attribute name="g1-parent-morph-gender">NULL</xsl:attribute>
                     <xsl:attribute name="g1-parent-morph-case">NULL</xsl:attribute>
                     <xsl:attribute name="g1-parent-morph-degree">NULL</xsl:attribute>
                 </xsl:otherwise>
                 
                 
             </xsl:choose>
            
            
           
            
            
            
            
            <xsl:variable name="g2-parent_id"
                select="parent::sentence/word[@id = $g1-parent_id]/@head"/>
            <xsl:variable name="g2-parent-pos" select="substring(../word[@id = $g2-parent_id]/@postag, 1, 1)"/>
            
            
           
            
            
            <xsl:choose>
                <xsl:when test="$g2-parent_id = 0">
                    <xsl:attribute name="g2-parent-relation">root</xsl:attribute>
                    <xsl:attribute name="g2-parent-depdist">NULL</xsl:attribute>
                    <xsl:attribute name="g2-parent-vertexDegree">NULL</xsl:attribute>
                    <xsl:attribute name="g2-parent-morph-pos">NULL</xsl:attribute>
                </xsl:when>
                <xsl:when test="$g2-parent_id > 0">
                    <xsl:attribute name="g2-parent-relation"><xsl:value-of select="../word[@id = $g2-parent_id]/@relation"/></xsl:attribute>
                    <xsl:attribute name="g2-parent-depdist"><xsl:value-of select="../word[@id = $g2-parent_id]/@DepDist"/></xsl:attribute>
                    <xsl:attribute name="g2-parent-vertexDegree"><xsl:value-of select="../word[@id = $g2-parent_id]/@Degree"/></xsl:attribute>
                    
                    <xsl:choose>
                        <xsl:when test="$g2-parent-pos='d'">
                            <xsl:attribute name="g2-parent-morph-pos">adverb</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g2-parent-pos='r'">
                            <xsl:attribute name="g2-parent-morph-pos">preposition</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g2-parent-pos='l'">
                            <xsl:attribute name="g2-parent-morph-pos">article</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g2-parent-pos='v'">
                            <xsl:attribute name="g2-parent-morph-pos">verb</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g2-parent-pos='a'">
                            <xsl:attribute name="g2-parent-morph-pos">adjective</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g2-parent-pos='p'">
                            <xsl:attribute name="g2-parent-morph-pos">pronoun</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g2-parent-pos='n'">
                            <xsl:attribute name="g2-parent-morph-pos">noun</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g2-parent-pos='c'">
                            <xsl:attribute name="g2-parent-morph-pos">conjunction</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g2-parent-pos='u'">
                            <xsl:attribute name="g2-parent-morph-pos">punctuation</xsl:attribute>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:attribute name="g2-parent-morph-pos">NA</xsl:attribute>
                        </xsl:otherwise>
                    </xsl:choose>    
                    
                    
                   
                    
                </xsl:when>
                <xsl:otherwise>
                    <xsl:attribute name="g2-parent-relation">NULL</xsl:attribute>
                    <xsl:attribute name="g2-parent-depdist">NULL</xsl:attribute>
                    <xsl:attribute name="g2-parent-vertexDegree">NULL</xsl:attribute>
                    <xsl:attribute name="g2-parent-morph-pos">NULL</xsl:attribute>
                </xsl:otherwise>
            </xsl:choose>
            
            
            
            
            <xsl:variable name="g3-parent_id"
                select="parent::sentence/word[@id = $g2-parent_id]/@head"/>
            <xsl:variable name="g3-parent-pos" select="substring(../word[@id = $g3-parent_id]/@postag, 1, 1)"/>
            
            
            <xsl:choose>
                <xsl:when test="$g3-parent_id = 0">
                    <xsl:attribute name="g3-parent-relation">root</xsl:attribute>
                    <xsl:attribute name="g3-parent-depdist">NULL</xsl:attribute>
                    <xsl:attribute name="g3-parent-vertexDegree">NULL</xsl:attribute>
                    <xsl:attribute name="g3-parent-morph-pos">NULL</xsl:attribute>
                </xsl:when>
                <xsl:when test="$g3-parent_id > 0">
                    <xsl:attribute name="g3-parent-relation"><xsl:value-of select="../word[@id = $g3-parent_id]/@relation"/></xsl:attribute>
                    <xsl:attribute name="g3-parent-depdist"><xsl:value-of select="../word[@id = $g3-parent_id]/@DepDist"/></xsl:attribute>
                    <xsl:attribute name="g3-parent-vertexDegree"><xsl:value-of select="../word[@id = $g3-parent_id]/@Degree"/></xsl:attribute>
                    
                    <xsl:choose>
                        <xsl:when test="$g3-parent-pos='d'">
                            <xsl:attribute name="g3-parent-morph-pos">adverb</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g3-parent-pos='r'">
                            <xsl:attribute name="g3-parent-morph-pos">preposition</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g3-parent-pos='l'">
                            <xsl:attribute name="g3-parent-morph-pos">article</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g3-parent-pos='v'">
                            <xsl:attribute name="g3-parent-morph-pos">verb</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g3-parent-pos='a'">
                            <xsl:attribute name="g3-parent-morph-pos">adjective</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g3-parent-pos='p'">
                            <xsl:attribute name="g3-parent-morph-pos">pronoun</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g3-parent-pos='n'">
                            <xsl:attribute name="g3-parent-morph-pos">noun</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g3-parent-pos='c'">
                            <xsl:attribute name="g3-parent-morph-pos">conjunction</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g2-parent-pos='u'">
                            <xsl:attribute name="g3-parent-morph-pos">punctuation</xsl:attribute>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:attribute name="g3-parent-morph-pos">NA</xsl:attribute>
                        </xsl:otherwise>
                    </xsl:choose>
                    
               
                    
                </xsl:when>
                <xsl:otherwise>
                    <xsl:attribute name="g3-parent-relation">NULL</xsl:attribute>
                    <xsl:attribute name="g3-parent-depdist">NULL</xsl:attribute>
                    <xsl:attribute name="g3-parent-vertexDegree">NULL</xsl:attribute>
                    <xsl:attribute name="g3-parent-morph-pos">NULL</xsl:attribute>
                </xsl:otherwise>
            </xsl:choose>
            
            
            <xsl:variable name="g4-parent_id"
                select="parent::sentence/word[@id = $g3-parent_id]/@head"/>
            <xsl:variable name="g4-parent-pos" select="substring(../word[@id = $g4-parent_id]/@postag, 1, 1)"/>
            
            
            <xsl:choose>
                <xsl:when test="$g4-parent_id = 0">
                    <xsl:attribute name="g4-parent-relation">root</xsl:attribute>
                    <xsl:attribute name="g4-parent-depdist">NULL</xsl:attribute>
                    <xsl:attribute name="g4-parent-vertexDegree">NULL</xsl:attribute>
                    <xsl:attribute name="g4-parent-morph-pos">NULL</xsl:attribute>
                </xsl:when>
                <xsl:when test="$g4-parent_id > 0">
                    <xsl:attribute name="g4-parent-relation"><xsl:value-of select="../word[@id = $g4-parent_id]/@relation"/></xsl:attribute>
                    <xsl:attribute name="g4-parent-depdist"><xsl:value-of select="../word[@id = $g4-parent_id]/@DepDist"/></xsl:attribute>
                    <xsl:attribute name="g4-parent-vertexDegree"><xsl:value-of select="../word[@id = $g4-parent_id]/@Degree"/></xsl:attribute>
                    
                    <xsl:choose>
                        <xsl:when test="$g4-parent-pos='r'">
                            <xsl:attribute name="g4-parent-morph-pos">preposition</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g4-parent-pos='d'">
                            <xsl:attribute name="g4-parent-morph-pos">adverb</xsl:attribute>
                        </xsl:when>                        
                        <xsl:when test="$g4-parent-pos='l'">
                            <xsl:attribute name="g4-parent-morph-pos">article</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g4-parent-pos='v'">
                            <xsl:attribute name="g4-parent-morph-pos">verb</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g4-parent-pos='a'">
                            <xsl:attribute name="g4-parent-morph-pos">adjective</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g4-parent-pos='p'">
                            <xsl:attribute name="g4-parent-morph-pos">pronoun</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g4-parent-pos='n'">
                            <xsl:attribute name="g4-parent-morph-pos">noun</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g4-parent-pos='c'">
                            <xsl:attribute name="g4-parent-morph-pos">conjunction</xsl:attribute>
                        </xsl:when>
                        <xsl:when test="$g4-parent-pos='u'">
                            <xsl:attribute name="g4-parent-morph-pos">punctuation</xsl:attribute>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:attribute name="g4-parent-morph-pos">NA</xsl:attribute>
                        </xsl:otherwise>
                    </xsl:choose>
                    
                    
                </xsl:when>
                <xsl:otherwise>
                    <xsl:attribute name="g4-parent-relation">NULL</xsl:attribute>
                    <xsl:attribute name="g4-parent-depdist">NULL</xsl:attribute>
                    <xsl:attribute name="g4-parent-vertexDegree">NULL</xsl:attribute>
                    <xsl:attribute name="g4-parent-morph-pos">NULL</xsl:attribute>
                </xsl:otherwise>
            </xsl:choose>
            
            
            
        </word>
    </xsl:template>
    
</xsl:stylesheet>