# iFrame Embedding Guide

## Overview

The Mpox Dashboard supports embedding individual charts in external dashboards (e.g., Superset, Tableau, Power BI) using URL query parameters. Each chart can be displayed standalone in an iframe with optional filters.

## Quick Start

### Basic Usage

```html
<!-- Embed pyramid chart -->
<iframe
  src="http://your-app-url/?chart=pyramid"
  width="100%"
  height="600px"
  frameborder="0">
</iframe>
```

### Clean Embed (No Header/Filters)

```html
<iframe
  src="http://your-app-url/?chart=pyramid&hide_header=true&hide_filters=true"
  width="100%"
  height="600px"
  frameborder="0">
</iframe>
```

## Available Charts

### Overview Tab

| Chart Name | Description | Recommended Height |
|------------|-------------|-------------------|
| `value_boxes` | All three value boxes (Total, Male, Female) | 200px |
| `total_cases` | Total cases value box only | 150px |
| `male_cases` | Male cases value box only | 150px |
| `female_cases` | Female cases value box only | 150px |
| `pyramid` | Age-sex distribution pyramid | 600px |
| `overview_summary` | Summary statistics table | 300px |

### Age Analysis Tab

| Chart Name | Description | Recommended Height |
|------------|-------------|-------------------|
| `violin_plot` | Age distribution violin plot by sex | 550px |
| `age_group_bar` | Age group distribution bar chart | 500px |
| `age_stats` | Age statistics table by sex | 250px |

### Geographic Tab

| Chart Name | Description | Recommended Height |
|------------|-------------|-------------------|
| `map` | Geographic choropleth map | 700px |
| `top_provinces` | Top provinces bar chart | 550px |
| `province_table` | Province statistics table | 400px |

### Analytics Tab

| Chart Name | Description | Recommended Height |
|------------|-------------|-------------------|
| `density_curve` | Age density distribution curves | 550px |
| `boxplot` | Age distribution boxplot | 550px |
| `data_table` | Full searchable data table | 600px |

## URL Parameters

### Required Parameters

- `chart` - The chart name to display (see table above)

### Optional Display Parameters

- `hide_header` - Hide the chart title header (default: `false`)
  - Values: `true`, `false`
  - Example: `?chart=pyramid&hide_header=true`

- `hide_filters` - Hide sidebar filters (default: `false`)
  - Values: `true`, `false`
  - Example: `?chart=pyramid&hide_filters=true`

### Optional Filter Parameters

Apply data filters via URL:

- `provinces` - Filter by specific provinces (comma-separated)
  - Example: `?chart=pyramid&provinces=SOUTH_KIVU,SANKURU`

- `sex` - Filter by sex
  - Values: `All`, `Male`, `Female`, `Unknown`
  - Example: `?chart=pyramid&sex=Male`

- `age_min` - Minimum age filter
  - Example: `?chart=pyramid&age_min=5`

- `age_max` - Maximum age filter
  - Example: `?chart=pyramid&age_max=50`

## Examples

### Example 1: Embed Total Cases (Clean)

```html
<iframe
  src="http://your-app-url/?chart=total_cases&hide_header=true"
  width="400px"
  height="150px"
  frameborder="0"
  style="border-radius: 8px;">
</iframe>
```

**Result**: Clean value box showing total case count

### Example 2: Embed Pyramid with Filter

```html
<iframe
  src="http://your-app-url/?chart=pyramid&provinces=SOUTH_KIVU&hide_header=true"
  width="100%"
  height="600px"
  frameborder="0">
</iframe>
```

**Result**: Age-sex pyramid showing only South Kivu data

### Example 3: Embed Age Distribution (Males Only)

```html
<iframe
  src="http://your-app-url/?chart=violin_plot&sex=Male&age_min=0&age_max=50"
  width="100%"
  height="550px"
  frameborder="0">
</iframe>
```

**Result**: Violin plot showing age distribution for males aged 0-50

### Example 4: Embed Multiple Charts in Grid

```html
<div style="display: grid; grid-template-columns: 1fr 1fr; gap: 20px;">
  <!-- Total Cases -->
  <iframe
    src="http://your-app-url/?chart=total_cases&hide_header=true"
    height="150px"
    frameborder="0">
  </iframe>

  <!-- Male Cases -->
  <iframe
    src="http://your-app-url/?chart=male_cases&hide_header=true"
    height="150px"
    frameborder="0">
  </iframe>

  <!-- Pyramid (full width) -->
  <iframe
    src="http://your-app-url/?chart=pyramid&hide_header=true"
    height="600px"
    frameborder="0"
    style="grid-column: span 2;">
  </iframe>
</div>
```

## Superset Integration

### Add as iFrame

1. In Superset, create a new dashboard
2. Add **"Markdown"** component
3. Use this code:

```html
<iframe
  src="http://your-shiny-app.com/?chart=pyramid&hide_header=true&hide_filters=true"
  width="100%"
  height="600px"
  frameborder="0"
  scrolling="no">
</iframe>
```

4. Adjust height based on chart type
5. Save and view

### Dynamic Filters (Advanced)

To sync Superset filters with the Shiny app, use Superset's dashboard parameters:

```html
<iframe
  src="http://your-app.com/?chart=pyramid&provinces={{ filter_province }}"
  width="100%"
  height="600px">
</iframe>
```

## Responsive Design

### Make Charts Responsive

```html
<div style="position: relative; width: 100%; padding-bottom: 60%;">
  <iframe
    src="http://your-app-url/?chart=pyramid&hide_header=true"
    style="position: absolute; top: 0; left: 0; width: 100%; height: 100%;"
    frameborder="0">
  </iframe>
</div>
```

### Recommended Aspect Ratios

| Chart Type | Aspect Ratio | padding-bottom |
|------------|--------------|----------------|
| Value boxes | 16:3 | 18.75% |
| Pyramid | 16:10 | 62.5% |
| Bar charts | 16:9 | 56.25% |
| Maps | 16:14 | 87.5% |
| Tables | Auto | - |

## Styling

### Add Custom Styling

```html
<iframe
  src="http://your-app-url/?chart=total_cases&hide_header=true"
  width="400px"
  height="150px"
  frameborder="0"
  style="
    border: 2px solid #dee2e6;
    border-radius: 12px;
    box-shadow: 0 4px 6px rgba(0,0,0,0.1);
  ">
</iframe>
```

## Security Considerations

1. **CORS**: Ensure your Shiny app allows embedding
   - May need to configure server headers

2. **Authentication**: iFrame inherits authentication
   - If main app requires auth, iframe will too

3. **HTTPS**: Use HTTPS for secure embedding
   - Some platforms require HTTPS iframes

## Troubleshooting

### Chart Not Displaying

**Problem**: Blank iframe or error message

**Solutions**:
1. Check chart name is correct (case-sensitive)
2. Verify URL is accessible
3. Check browser console for errors
4. Ensure database connection is working

### Chart Cut Off

**Problem**: Chart is partially visible

**Solutions**:
1. Increase iframe height
2. Remove `hide_header=true` to see full content
3. Check chart's recommended height in tables above

### Filters Not Working

**Problem**: URL filter parameters not applied

**Solutions**:
1. Check parameter syntax: `&provinces=VALUE` not `&province=VALUE`
2. Use comma-separated values for multiple items
3. Verify parameter values match data (case-sensitive)

## Testing URLs

### Test Individual Charts

Visit these URLs directly in your browser:

```
http://your-app-url/?chart=total_cases
http://your-app-url/?chart=pyramid
http://your-app-url/?chart=violin_plot
http://your-app-url/?chart=map
```

### Test with Filters

```
http://your-app-url/?chart=pyramid&sex=Male
http://your-app-url/?chart=pyramid&provinces=SOUTH_KIVU,SANKURU
http://your-app-url/?chart=violin_plot&age_min=0&age_max=18
```

### Test Clean Embeds

```
http://your-app-url/?chart=pyramid&hide_header=true&hide_filters=true
```

## Best Practices

1. **Always specify height**: Each chart type has optimal height
2. **Use hide_header=true for embeds**: Cleaner appearance
3. **Test responsiveness**: Check on different screen sizes
4. **Combine related charts**: Group in same dashboard section
5. **Document your URLs**: Keep track of embedded chart URLs
6. **Monitor performance**: Embedded charts make live DB queries

## Support

For issues or questions:
1. Check this guide first
2. Test URL directly in browser
3. Check application logs
4. Review query parameter syntax

---

**Version**: 1.0
**Last Updated**: 2025-11-02
