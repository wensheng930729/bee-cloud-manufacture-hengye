import React, { useEffect } from 'react';
import { FormItemRangePicker, FormItemSelect, FormItemInputSearch } from '@/components/FormWidget';
import { SearchFormHOC, TableForParentTab } from '@/components/TableForParentTab';
import { reportType as REPORT_TYPE } from '@/consts/reportForm';
import BaseComponent from '../BaseComponent';

const params = {
  name: 'huhao',
};

const MyFormSearch = props => {
  const { getFieldDecorator, productsOptions, productsCateOptions, hasCate } = props;
  const formItemLayout = {};

  return (
    <>
      <FormItemInputSearch
        getFieldDecorator={getFieldDecorator}
        label="样品编号"
        fieldId="sampleCode"
        required={false}
        inputProps={{ placeholder: '按取样单编号搜索' }}
        formItemLayout={formItemLayout}
      />
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="产品"
        fieldId="productId"
        required={false}
        selectProps={{ options: productsOptions }}
        formItemLayout={formItemLayout}
      />
      {hasCate ? (
        <FormItemSelect
          getFieldDecorator={getFieldDecorator}
          label="产品类别"
          fieldId="categoryId"
          required={false}
          selectProps={{ options: productsCateOptions }}
          formItemLayout={formItemLayout}
        />
      ) : null}

      <FormItemRangePicker
        getFieldDecorator={getFieldDecorator}
        label="创建日期"
        fieldId="dateRange"
        required={false}
        datePickerProps={{ format: 'YYYY-MM-DD' }}
        formItemLayout={formItemLayout}
      />
    </>
  );
};

const NewIndex = SearchFormHOC(params)(MyFormSearch);

const NewCom = TableForParentTab(params)(NewIndex);

const Index = () => {
  useEffect(() => {
    try {
      _czc1.push(['_trackEvent', '查看报表', '查看质检报表', '', '', '']);
    } catch (error) {}
  });

  const defaultTabPagesParams = {
    type: 'quality_test',
    columnsParams: {
      businessType: REPORT_TYPE.productQualiInspect.businessType,
      reportType: REPORT_TYPE.productQualiInspect.key,
    },
  };
  return (
    <BaseComponent defaultTabPagesParams={defaultTabPagesParams}>
      <NewCom />
    </BaseComponent>
  );
};

export default Index;

/*
      hasCate:
        listSearchParams.reportType === REPORT_TYPE.entryExitQualiInspect.key,
*/
