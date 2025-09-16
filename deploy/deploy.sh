#!/bin/bash
#=============================================================================
# SAP ABAP SY-UNAME Tracker - Deployment Script (Unix/Linux/Mac)
#=============================================================================

echo "========================================="
echo "SAP ABAP SY-UNAME Tracker Deployment"
echo "========================================="

# Check Python installation
if ! command -v python3 &> /dev/null; then
    echo "Error: Python 3 is not installed"
    echo "Please install Python 3 and try again"
    exit 1
fi

# Create output directory
mkdir -p deploy/output

# Run deployment generator
echo ""
echo "Generating deployment package..."
python3 deploy/deploy_to_sap.py

# Check if successful
if [ $? -eq 0 ]; then
    echo ""
    echo "========================================="
    echo "✓ Deployment package ready!"
    echo "========================================="
    echo ""
    echo "Manual deployment steps:"
    echo "1. Log into SAP GUI"
    echo "2. Go to SE80 and create package Z_SYUNAME_TRACKER"
    echo "3. Import the generated XML file using SAPLink (ZSAPLINK)"
    echo "   OR"
    echo "   Create each object manually using SE80/SE38"
    echo ""
    echo "Files are in: deploy/output/"
    ls -la deploy/output/
else
    echo "Error: Deployment generation failed"
    exit 1
fi