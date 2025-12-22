# ShinyAIM v2.0 - Testing Guide

## Prerequisites
Ensure all required packages are installed:
```r
install.packages(c("shiny","ggplot2","dplyr","grid","plotly","manhattanly","forcats",
                   "DT","shinycssloaders","bslib","colourpicker","shinyWidgets",
                   "ggrepel","scales","viridis"))
```

## Running the Application

### Local Testing
```r
# Option 1: Run from directory
setwd("/path/to/ShinyAIM")
shiny::runApp()

# Option 2: Run from GitHub
shiny::runGitHub("ShinyAIM", "whussain2")
```

## Feature Testing Checklist

### 1. Interactive Manhattan Plot Tab
- [ ] Upload sample file: `Samplefiles/samplefile1_manhattan.csv`
- [ ] Select timepoint from dropdown
- [ ] Verify plot displays correctly
- [ ] Check summary statistics boxes show:
  - [ ] Total Markers count
  - [ ] Significant Markers count
  - [ ] Lambda (λ) value
  - [ ] Threshold value
- [ ] Test Color Scheme selector:
  - [ ] Default colors
  - [ ] Colorblind-Friendly (should use viridis colors)
  - [ ] Grayscale (should show black/gray tones)
  - [ ] Custom (test color pickers)
- [ ] Test Multiple Testing Correction:
  - [ ] None (manual threshold)
  - [ ] Bonferroni (threshold should change)
  - [ ] FDR (threshold should change)
- [ ] Adjust significance threshold slider
- [ ] Check "Display in Table Significant SNPs" checkbox
- [ ] Verify DT interactive table appears:
  - [ ] Sortable columns
  - [ ] Searchable
  - [ ] Shows correct number of SNPs
- [ ] Test export buttons:
  - [ ] Download PNG (check file opens)
  - [ ] Download PDF (check file opens)
  - [ ] Download SVG (check file opens)

### 2. QQ Plot Tab
- [ ] Verify timepoint selector appears
- [ ] Select a timepoint
- [ ] Verify QQ plot displays
- [ ] Check lambda (λ) value displays
- [ ] Verify interpretation text appears
- [ ] Check points fall along diagonal line
- [ ] Test interactivity (hover over points)
- [ ] Test export buttons:
  - [ ] Export PNG
  - [ ] Export PDF

### 3. Manhattan Grid Plot Tab
- [ ] Upload sample file: `Samplefiles/samplefile1_manhattan.csv`
- [ ] Verify grid plot displays with multiple panels
- [ ] Adjust number of columns slider (2-10)
- [ ] Verify grid layout changes
- [ ] Adjust p-value threshold
- [ ] Test data preprocessing inputs:
  - [ ] MAF threshold (UI only, no effect expected)
  - [ ] Missing rate threshold (UI only, no effect expected)
- [ ] Verify loading spinner appears during plot generation
- [ ] Test "Export Grid Plot" button

### 4. Comparison of Associated Markers Tab
- [ ] Note: Uses data from Manhattan Grid Plot
- [ ] Enter p-value threshold (e.g., 0.000001)
- [ ] Verify comparison plot displays
- [ ] Check different timepoints show different colors/shapes
- [ ] Test interactivity

### 5. Data Management Tab
- [ ] Session Save:
  - [ ] Enter a session name
  - [ ] Click "Save Session (.RDS)"
  - [ ] Verify .rds file downloads
- [ ] Session Info:
  - [ ] Check "Session Information" displays correctly
  - [ ] Verify loaded files are listed
  - [ ] Check marker and timepoint counts
- [ ] Multiple File Comparison:
  - [ ] Upload first GWAS file
  - [ ] Upload second GWAS file
  - [ ] Check "Show Comparison Plot"
  - [ ] Verify side-by-side plots appear
  - [ ] Compare visual patterns between files

### 6. Phenotypic Data Visualization Tab
- [ ] Download sample file using button
- [ ] Upload `Samplefiles/samplefile2_phenotypic.csv`
- [ ] Select timepoint
- [ ] Test all plot types:
  - [ ] Histogram (shows distribution with mean line)
  - [ ] Density (shows density curve)
  - [ ] DensityAll (shows all timepoints overlaid)
  - [ ] Boxplot (shows boxes for all timepoints)
- [ ] Verify loading spinner appears

### 7. Information Tab
- [ ] Verify page loads correctly
- [ ] Check new features are documented
- [ ] Verify guidelines are updated
- [ ] Check all links work

## Browser Testing
Test in multiple browsers:
- [ ] Chrome/Chromium
- [ ] Firefox
- [ ] Safari
- [ ] Edge

## Responsive Design Testing
Test at different screen sizes:
- [ ] Desktop (1920x1080)
- [ ] Laptop (1366x768)
- [ ] Tablet (768x1024)
- [ ] Large monitor (2560x1440)

## Performance Testing
- [ ] Test with small dataset (~1000 markers)
- [ ] Test with medium dataset (~10000 markers)
- [ ] Test with large dataset (100000+ markers)
- [ ] Verify loading spinners appear for slow operations
- [ ] Check for memory leaks (multiple uploads/plots)

## Error Handling
Test error conditions:
- [ ] Upload invalid file format
- [ ] Upload file with missing columns
- [ ] Upload empty file
- [ ] Select timepoint before file upload
- [ ] Adjust settings with no data loaded

## Console Testing
Open browser developer console (F12) and check for:
- [ ] No JavaScript errors
- [ ] No warning messages
- [ ] Plotly loads correctly
- [ ] DT tables initialize properly

## Package Dependencies Check
Verify all packages load:
```r
# Run this in R console
packages <- c("shiny", "ggplot2", "dplyr", "grid", "plotly", "manhattanly", 
              "forcats", "DT", "shinycssloaders", "bslib", "colourpicker", 
              "shinyWidgets", "ggrepel", "scales", "viridis")

for(pkg in packages) {
  if(!require(pkg, character.only = TRUE)) {
    cat(sprintf("MISSING: %s\n", pkg))
  } else {
    cat(sprintf("OK: %s\n", pkg))
  }
}
```

## Common Issues and Solutions

### Issue: Packages not installing
**Solution**: Install dependencies manually:
```r
install.packages(c("shiny","ggplot2","dplyr","grid","plotly","manhattanly","forcats"))
install.packages(c("DT","shinycssloaders","bslib","colourpicker","shinyWidgets"))
install.packages(c("ggrepel","scales","viridis"))
```

### Issue: Plot not displaying
**Solution**: 
1. Check browser console for errors
2. Verify data file format matches sample files
3. Ensure all required columns present (timepoint, marker, chrom, pos, P)

### Issue: Color picker not working
**Solution**: Ensure colourpicker package is properly installed:
```r
remove.packages("colourpicker")
install.packages("colourpicker")
```

### Issue: Download buttons not working
**Solution**: Check browser popup blocker settings

### Issue: DT table not interactive
**Solution**: Verify DT package version:
```r
packageVersion("DT")  # Should be >= 0.20
```

## Regression Testing
After any code changes, verify:
- [ ] Original sample files still work
- [ ] All original features functional
- [ ] No new error messages
- [ ] Performance hasn't degraded
- [ ] Backward compatibility maintained

## Accessibility Testing
- [ ] Test with keyboard navigation
- [ ] Verify color contrast ratios
- [ ] Check screen reader compatibility (if available)
- [ ] Test color-blind friendly palette

## Documentation Verification
- [ ] README.md instructions are accurate
- [ ] Information tab matches actual features
- [ ] Sample files are included and work
- [ ] Installation instructions are complete

## Reporting Issues
When reporting bugs, include:
1. Browser and version
2. R version: `R.version.string`
3. Package versions: `sessionInfo()`
4. Steps to reproduce
5. Error messages (if any)
6. Screenshots (if applicable)

## Success Criteria
Application is ready for release when:
- [ ] All features in checklist work as expected
- [ ] No critical errors in browser console
- [ ] Sample files work correctly
- [ ] Documentation is accurate and complete
- [ ] Export functions produce valid files
- [ ] Performance is acceptable for typical datasets
- [ ] UI is responsive and professional-looking
