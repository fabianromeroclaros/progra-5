import java.util.Arrays;

public class QuickSort {
    public static void main(String[] args) {
        int[] arr = { 12, 7, 11, 3, 9, 2, 6 };
        quickSort(arr, 0, arr.length - 1);
        System.out.println("Sorted array: " + Arrays.toString(arr));
    }

    public static void quickSort(int[] arr, int low, int high) {
        if (low < high) {
            int pivotIndex = partition(arr, low, high);
            quickSort(arr, low, pivotIndex - 1);
            quickSort(arr, pivotIndex + 1, high);
        }
    }

    public static int partition(int[] arr, int low, int high) {
        int pivot = arr[high];
        int pivotIndex = low;

        for (int j = low; j < high; j++) {
            if (arr[j] <= pivot) {
                swap(arr, pivotIndex, j);
                pivotIndex++;
            }
        }

        swap(arr, pivotIndex, high);
        return pivotIndex;
    }

    public static void swap(int[] arr, int i, int j) {
        int temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
    }
}
