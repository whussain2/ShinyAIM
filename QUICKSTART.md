# ShinyAIM v2.0 - Quick Reference Card

## 🚀 New Features at a Glance

### 📊 Enhanced Manhattan Plots
- **4 Color Schemes**: Default, Colorblind-Friendly, Grayscale, Custom
- **Statistical Corrections**: Bonferroni, FDR, or None
- **Live Stats**: Total markers, significant hits, λ, thresholds
- **Export**: PNG, PDF, SVG formats

### 📈 QQ Plots (NEW!)
- Assess p-value distribution
- Genomic inflation factor (λ)
- Detect population stratification
- Interactive with tooltips
- Export PNG/PDF

### 📑 Interactive Tables (NEW!)
- Search and filter SNPs
- Sort by any column
- Pagination
- Export to CSV

### 💾 Data Management (NEW!)
- Save/load analysis sessions
- Compare two GWAS runs
- Session information display

### 🎨 Modern UI
- Professional theme
- Loading animations
- Color-coded stats
- Responsive design

## 🎯 Quick Start Guide

### 1. Launch Application
```r
# From GitHub
shiny::runGitHub("ShinyAIM", "whussain2")

# Or locally
setwd("/path/to/ShinyAIM")
shiny::runApp()
```

### 2. Upload Your Data
**Required Columns:**
- `timepoint` - Time point or phenotype identifier
- `marker` - SNP/marker name
- `chrom` - Chromosome number
- `pos` - Base pair position
- `P` - P-value

**Optional Columns:**
- `beta` - Effect size (for future features)
- `MAF` - Minor allele frequency

### 3. Explore Features

#### Interactive Manhattan Plot
1. Upload CSV file
2. Select timepoint
3. Choose color scheme
4. Select correction method
5. Adjust significance threshold
6. Export plot (PNG/PDF/SVG)

#### QQ Plot
1. Same data as Manhattan plot
2. Select timepoint
3. View λ and interpretation
4. Export plot

#### Grid Manhattan Plot
1. Upload CSV file
2. Adjust columns (2-10)
3. Set significance threshold
4. Export combined plot

#### Compare Files
1. Go to Data Management tab
2. Upload File 1 and File 2
3. Check "Show Comparison Plot"
4. View side-by-side

## 🎨 Color Schemes

### Default
- Brown, Purple, Steel Blue, Yellow-Green
- Original ShinyAIM colors

### Colorblind-Friendly
- Viridis palette
- Optimized for all color vision types

### Grayscale
- Black to light gray
- Perfect for publications

### Custom
- Choose your own 4 colors
- Use color pickers

## 📊 Statistical Methods

### Multiple Testing Corrections

**Bonferroni**
- Most conservative
- Threshold = α / number of tests
- Example: 0.05 / 1,000,000 = 5×10⁻⁸

**FDR (Benjamini-Hochberg)**
- Controls false discovery rate
- Less conservative than Bonferroni
- Good for exploratory analysis

**None**
- Manual threshold setting
- Use slider to adjust

### Genomic Inflation Factor (λ)

**Interpretation:**
- λ ≈ 1.0: Well-controlled ✅
- λ > 1.1: Possible inflation ⚠️
- λ < 0.9: Possible deflation ⚠️

## 📥 Export Options

### Manhattan Plots
- **PNG**: High-res (1200×600, 150 DPI)
- **PDF**: Vector, scalable, publication-ready
- **SVG**: Editable in Illustrator/Inkscape

### QQ Plots
- **PNG**: Square (800×800, 150 DPI)
- **PDF**: Vector format

### Grid Plots
- **PNG**: Large (1600×1200, 150 DPI)

### Sessions
- **RDS**: Complete analysis state
- Reload anytime

## 🔍 Interactive Table Features

### Search
- Type in search box
- Searches all columns
- Real-time filtering

### Sort
- Click column headers
- Multi-column sort (Shift+Click)
- Ascending/descending

### Filter
- Custom filters per column
- Numeric ranges
- Text matching

### Export
- Copy button
- CSV export
- Print view

## 💡 Tips & Tricks

### Performance
- Large files (>100K markers) may be slow
- Use data preprocessing to filter
- Close other applications if needed

### Visualization
- Zoom in plots with drag
- Double-click to reset zoom
- Hover for details

### Color Selection
- Test colorblind-friendly for publications
- Use grayscale for print journals
- Custom colors for presentations

### File Comparison
- Use same chromosome set
- Same assembly version
- Compare similar traits

## ⚠️ Common Issues

### "Package not found"
**Solution:**
```r
install.packages(c("DT","shinycssloaders","bslib",
                   "colourpicker","shinyWidgets",
                   "ggrepel","scales","viridis"))
```

### Plot not showing
**Check:**
- File format (CSV)
- Required columns present
- No missing headers
- Separator matches data

### Download not working
**Check:**
- Browser popup blocker
- Download permissions
- Disk space

### Slow performance
**Try:**
- Filter data before upload
- Use smaller timepoint subset
- Close other browser tabs

## 📚 Sample Data Format

```csv
timepoint,marker,chrom,pos,P
1,SNP-1.131482,1,132483,0.000342505
1,SNP-1.686571,1,687572,0.00030756
2,SNP-1.866303,1,867304,0.000670995
```

### Phenotypic Data Format
```csv
NSFTV_ID,timepoint,Value
NSFTV_10,1,0.86
NSFTV_101,1,0.92
```

## 🔗 Useful Links

- **GitHub**: https://github.com/whussain2/ShinyAIM
- **Paper**: https://doi.org/10.1002/pld3.91
- **Hosted App**: https://chikudaisei.shinyapps.io/shinyaim/

## 📧 Support

**Questions?** Contact:
- Waseem Hussain: waseemhussain907@gmail.com
- GitHub Issues: Report bugs and request features

## 🎓 Citation

When using ShinyAIM, please cite:

> Hussain W, Campbell M, Walia H, Morota G. ShinyAIM: Shiny‐based application 
> of interactive Manhattan plots for longitudinal genome‐wide association studies. 
> Plant Direct. 2018;2:1–4. https://doi.org/10.1002/pld3.91

## 📝 Version History

- **v2.0** (2024): Major enhancement with QQ plots, new UI, data management
- **v1.0** (2018): Original release with basic Manhattan plots

---

**Pro Tip**: Use the Information tab in the app for detailed guidelines and help!
