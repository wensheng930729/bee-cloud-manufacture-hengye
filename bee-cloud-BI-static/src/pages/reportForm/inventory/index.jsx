import React, { useEffect } from 'react';
import { FormItemRangePicker, FormItemSelect, FormItemDatePicker } from '@/components/FormWidget';
import { SearchFormHOC, TableForParentTab } from '@/components/TableForParentTab';
import { reportType as REPORT_TYPE } from '@/consts/reportForm';
import BaseComponent from '../BaseComponent';

const params = {
  name: 'huhao',
};

const MyFormSearch = props => {
  const { getFieldDecorator, productsOptions, storageOptions, nostorageId } = props;
  const formItemLayout = {};

  return (
    <>
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="产品"
        fieldId="productId"
        required={false}
        selectProps={{ options: productsOptions }}
        formItemLayout={formItemLayout}
      />

      {nostorageId ? (
        <FormItemDatePicker
          getFieldDecorator={getFieldDecorator}
          label="查询时间"
          fieldId="dayTime"
          required={false}
          formItemLayout={formItemLayout}
          datePickerProps={{ format: 'YYYY-MM-DD' }}
        />
      ) : (
        <FormItemRangePicker
          getFieldDecorator={getFieldDecorator}
          label="查询时间"
          fieldId="dateRange"
          required={false}
          formItemLayout={formItemLayout}
          datePickerProps={{ format: 'YYYY-MM-DD' }}
        />
      )}

      {nostorageId ? null : (
        <FormItemSelect
          getFieldDecorator={getFieldDecorator}
          label="仓库"
          fieldId="storageId"
          required={false}
          selectProps={{ options: storageOptions }}
          formItemLayout={formItemLayout}
        />
      )}
    </>
  );
};

const NewIndex = SearchFormHOC(params)(MyFormSearch);

const NewCom = TableForParentTab(params)(NewIndex);

const Index = () => {
  useEffect(() => {
    try {
      _czc1.push(['_trackEvent', '查看报表', '查看库存', '', '', '']);
    } catch (error) {}
  });

  const defaultTabPagesParams = {
    type: 'stock',
    columnsParams: {
      reportType: REPORT_TYPE.existingDeatil.key,
      businessType: REPORT_TYPE.existingDeatil.businessType,
    },
  };
  return (
    <BaseComponent defaultTabPagesParams={defaultTabPagesParams}>
      <NewCom />
    </BaseComponent>
  );
};

export default Index;
